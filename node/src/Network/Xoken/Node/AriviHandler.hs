{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Xoken.Node.AriviHandler
    ( AriviNetworkServiceHandler(..)
    , newAriviNetworkServiceHandler
    , setupEndPointServer
    , handleNewConnectionRequest
    , PubSubMsg(..)
    ) where

import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types
import Codec.Serialise
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.Aeson as A
import Data.Binary as DB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Int
import qualified Data.Map.Strict as M
import Data.Serialize
import Data.Text as T
import Data.X509.CertificateStore
import GHC.Generics
import qualified Network.Simple.TCP.TLS as TLS
import Network.Socket
import qualified Network.TLS as NTLS
import Network.Xoken.Node.AriviService.Data
import Network.Xoken.Node.AriviService.Env
import Network.Xoken.Node.AriviService.Types
import Network.Xoken.Node.Data
import Network.Xoken.Node.Env as NEnv
import Text.Printf

data AriviNetworkServiceHandler =
    AriviNetworkServiceHandler
        { connQueue :: TQueue EndPointConnection
        }

data EndPointConnection =
    EndPointConnection
        { requestQueue :: TQueue XDataReq
        , respWriteLock :: MVar TLS.Context
        }

newAriviNetworkServiceHandler :: IO AriviNetworkServiceHandler
newAriviNetworkServiceHandler = do
    conQ <- atomically $ newTQueue
    return $ AriviNetworkServiceHandler conQ

newEndPointConnection :: IO EndPointConnection
newEndPointConnection = do
    reqQueue <- atomically $ newTQueue
    resLock <- newEmptyMVar
    return $ EndPointConnection reqQueue resLock

goGetResource ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => RPCMessage -> m (RPCMessage)
goGetResource msg = do
    liftIO $ print ("fetchResource")
    resource <- fetchResource (RpcPayload AriviSecureRPC msg)
    case resource of
        Left e -> do
            liftIO $ print ("Exception: No peers available to issue RPC" ++ show e)
            return (RPCResponse 400 (Just "No connected peers") Nothing)
        Right (RpcError _) -> return (RPCResponse 500 (Just "Unknown RPC error") Nothing)
        Right (RpcPayload _ respMsg) -> do
            liftIO $ print (respMsg)
            return (respMsg)

handleRPCReqResp ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage)
    => MVar TLS.Context
    -> Int
    -> RPCMessage
    -> m ()
handleRPCReqResp sockMVar mid encReq = do
    liftIO $ printf "handleRPCReqResp(%d, %s)\n" mid (show encReq)
    rpcResp <- goGetResource encReq
    let body = serialise $ XDataRPCResp (mid) (rsStatusCode rpcResp) (rsStatusMessage rpcResp) (rsBody rpcResp)
    connSock <- liftIO $ takeMVar sockMVar
    NTLS.sendData connSock $ DB.encode (Prelude.fromIntegral (LBS.length body) :: Int16)
    NTLS.sendData connSock body
    liftIO $ putMVar sockMVar connSock

handlePubSubReqResp ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage)
    => MVar TLS.Context
    -> PubSubMsg
    -> m ()
handlePubSubReqResp sockMVar sub = do
    liftIO $ print ("handleSubscribeReqResp " ++ show sub)
    -- INSERT
    return ()

handleNewConnectionRequest ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => AriviNetworkServiceHandler -> m ()
handleNewConnectionRequest handler = do
    continue <- liftIO $ newIORef True
    whileM_ (liftIO $ readIORef continue) $ do
        liftIO $ printf "handleNewConnectionRequest\n"
        epConn <- liftIO $ atomically $ readTQueue $ connQueue handler
        async $ handleRequest epConn

handleRequest ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => EndPointConnection -> m ()
handleRequest handler = do
    continue <- liftIO $ newIORef True
    whileM_ (liftIO $ readIORef continue) $ do
        liftIO $ printf "handleRequest\n"
        xdReq <- liftIO $ atomically $ readTQueue $ requestQueue handler
        case xdReq of
            XDataRPCReq mid met par -> do
                liftIO $ printf "Decoded (%s)\n" (show met)
                let req = RPCRequest met par
                async (handleRPCReqResp (respWriteLock handler) mid req)
                return ()
            XDataSubscribe top -> do
                async (handlePubSubReqResp (respWriteLock handler) (Subscribe' top))
                return ()
            XDataPublish top body -> do
                async (handlePubSubReqResp (respWriteLock handler) (Publish' top $ PubNotifyMessage body))
                return ()
            XCloseConnection -> do
                liftIO $ writeIORef continue False

enqueueRequest :: EndPointConnection -> LBS.ByteString -> IO ()
enqueueRequest epConn req = do
    let xdReq = deserialise req :: XDataReq
    atomically $ writeTQueue (requestQueue epConn) xdReq

handleConnection :: EndPointConnection -> TLS.Context -> IO ()
handleConnection epConn context = do
    continue <- liftIO $ newIORef True
    putMVar (respWriteLock epConn) context
    whileM_ (liftIO $ readIORef continue) $ do
        res <- try $ TLS.recv context
        case res of
            Right r ->
                case r of
                    Just l -> enqueueRequest epConn (LBS.fromStrict l)
                    Nothing -> putStrLn "End of Stream" -- Shouldn't be here
            Left (e :: IOException) -> do
                putStrLn "Connection closed."
                atomically $ writeTQueue (requestQueue epConn) XCloseConnection
                writeIORef continue False

setupEndPointServer :: AriviNetworkServiceHandler -> String -> PortNumber -> FilePath -> FilePath -> FilePath -> IO ()
setupEndPointServer handler listenIP listenPort certFilePath keyFilePath mStoreFilePath = do
    putStrLn $ "Starting Xoken Arch"
    cred <- NTLS.credentialLoadX509 certFilePath keyFilePath
    case cred of
        Right c -> do
            cstore <- readCertificateStore mStoreFilePath
            let settings = TLS.makeServerSettings c cstore
            TLS.serve settings (TLS.Host listenIP) (show listenPort) $ \(context, sockAddr) -> do
                putStrLn $ "client connection established : " ++ show sockAddr
                epConn <- newEndPointConnection
                atomically $ writeTQueue (connQueue handler) epConn
                handleConnection epConn context
        Left err -> do
            putStrLn $ "Unable to read credentials from file"
            error "BadCredentialFile"
