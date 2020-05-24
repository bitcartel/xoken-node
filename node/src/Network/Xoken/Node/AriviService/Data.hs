{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Xoken.Node.AriviService.Data where

import Codec.Serialise
import Control.Concurrent.MVar
import Control.Exception
import Data.ByteString
import Data.Hashable
import Data.Int
import GHC.Generics
import Network.Xoken.Block
import Network.Xoken.Node.Data
import Network.Xoken.Node.AriviService.Types

-- import Control.Concurrent.MVar
-- import Control.Concurrent.STM
-- import qualified Data.ByteString as B
-- import Data.Functor.Identity
-- import qualified Data.Map.Strict as M
-- import Network.Socket hiding (send)
-- import Data.Time.Clock
-- import Data.Word
data XDataReq
    = XDataRPCReq
          { reqId :: Int
          , method :: String
          , params :: Maybe RPCReqParams
          }
    | XDataSubscribe
          { subscribeTopic :: String
          }
    | XDataPublish
          { publishTopic :: String
          , publishBody :: String
          }
    | XCloseConnection
    deriving (Show, Generic, Hashable, Eq, Serialise)

data XDataResp
    = XDataRPCResp
          { matchId :: Int
          , statusCode :: Int16
          , statusMessage :: Maybe String
          , respBody :: Maybe RPCResponseBody
          }
    | XDataNotify
          { notifyTopic :: String
          , notifyBody :: String
          }
    deriving (Show, Generic, Hashable, Eq, Serialise) --
