{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Xoken.Node.AriviService.Env where

import Arivi.Env
import Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig, networkConfig)
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Env
import Arivi.P2P.PubSub.Publish as Pub
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types
import Codec.Serialise
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Hashable
import Data.Map.Strict as M
import GHC.Generics
import Network.Xoken.Node.AriviService.Types
import Network.Xoken.Node.Data
import qualified Network.Xoken.Node.Env as NEnv

data EndPointEnv =
  EndPointEnv
        -- tcpConn :: (Socket, SockAddr)
        -- , reqQueue :: TChan (EndPointMessage, (MVar EndPointMessage))
        -- , msgMatch :: TVar (M.Map Int (MVar EndPointMessage))
    {
    }

class HasEndPointEnv env where
  getEndPointEnv :: env -> EndPointEnv

instance HasEndPointEnv (ServiceEnv m r t rmsg pmsg) where
  getEndPointEnv = tcpEnv

data ServiceEnv m r t rmsg pmsg =
  ServiceEnv
    { tcpEnv :: EndPointEnv
    , p2pEnv :: P2PEnv m r t rmsg pmsg
    }

type HasService env m
   = ( HasP2PEnv env m NEnv.ServiceResource NEnv.ServiceTopic RPCMessage PubNotifyMessage
     , HasEndPointEnv env
     , MonadReader env m)

instance HasNetworkConfig (ServiceEnv m r t rmsg pmsg) NetworkConfig where
  networkConfig f se =
    fmap
      (\nc ->
         se
           { p2pEnv =
               (p2pEnv se)
                 { nodeEndpointEnv =
                     (nodeEndpointEnv (p2pEnv se))
                       {Arivi.P2P.P2PEnv._networkConfig = nc}
                 }
           })
      (f ((Arivi.P2P.P2PEnv._networkConfig . nodeEndpointEnv . p2pEnv) se))

instance HasTopics (ServiceEnv m r t rmsg pmsg) t where
  topics = pubSubTopics . psEnv . p2pEnv

instance HasSubscribers (ServiceEnv m r t rmsg pmsg) t where
  subscribers = pubSubSubscribers . psEnv . p2pEnv

instance HasNotifiers (ServiceEnv m r t rmsg pmsg) t where
  notifiers = pubSubNotifiers . psEnv . p2pEnv

instance HasPubSubEnv (ServiceEnv m r t rmsg pmsg) t where
  pubSubEnv = psEnv . p2pEnv

instance HasRpcEnv (ServiceEnv m r t rmsg pmsg) r rmsg where
  rpcEnv = rEnv . p2pEnv

instance HasPSGlobalHandler (ServiceEnv m r t rmsg pmsg) m r t rmsg pmsg where
  psGlobalHandler = psHandler . p2pEnv

instance HasRpcGlobalHandler (ServiceEnv m r t rmsg pmsg) m r t rmsg pmsg where
  rpcGlobalHandler = rHandler . p2pEnv

newtype AppM a =
  AppM
    (ReaderT (ServiceEnv AppM NEnv.ServiceResource NEnv.ServiceTopic RPCMessage PubNotifyMessage) (LoggingT IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (ServiceEnv AppM NEnv.ServiceResource NEnv.ServiceTopic RPCMessage PubNotifyMessage)
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadLogger
           )

deriving instance MonadBase IO AppM

deriving instance MonadBaseControl IO AppM

instance HasNetworkEnv AppM where
  getEnv = asks (ariviNetworkEnv . nodeEndpointEnv . p2pEnv)

instance HasSecretKey AppM

instance HasKbucket AppM where
  getKb = asks (kbucket . kademliaEnv . p2pEnv)

instance HasStatsdClient AppM where
  getStatsdClient = asks (statsdClient . p2pEnv)

instance HasNodeEndpoint AppM where
  getEndpointEnv = asks (nodeEndpointEnv . p2pEnv)
  getNetworkConfig = asks (_networkConfig . nodeEndpointEnv . p2pEnv)
  getHandlers = asks (handlers . nodeEndpointEnv . p2pEnv)
  getNodeIdPeerMapTVarP2PEnv =
    asks (tvarNodeIdPeerMap . nodeEndpointEnv . p2pEnv)

instance HasPRT AppM where
  getPeerReputationHistoryTableTVar =
    asks (tvPeerReputationHashTable . prtEnv . p2pEnv)
  getServicesReputationHashMapTVar =
    asks (tvServicesReputationHashMap . prtEnv . p2pEnv)
  getP2PReputationHashMapTVar = asks (tvP2PReputationHashMap . prtEnv . p2pEnv)
  getReputedVsOtherTVar = asks (tvReputedVsOther . prtEnv . p2pEnv)
  getKClosestVsRandomTVar = asks (tvKClosestVsRandom . prtEnv . p2pEnv)

runAppM ::
     ServiceEnv AppM NEnv.ServiceResource NEnv.ServiceTopic RPCMessage PubNotifyMessage
  -> AppM a
  -> LoggingT IO a
runAppM env (AppM app) = runReaderT app env
