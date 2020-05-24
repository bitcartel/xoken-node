{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Xoken.Node.AriviService.Types where

import Codec.Serialise
import Control.Concurrent.MVar
import Control.Exception
import Data.ByteString
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Hashable
import Data.Int
import GHC.Generics
import Network.Xoken.Block
import Network.Xoken.Node.Data

data EndpointException
    = InvalidMessageTypeException
    | MessageParsingException
    | UnsupportedMethodException
    deriving (Show)

data PeerMessageException
    = SocketReadException
    | ZeroLengthSocketReadException
    deriving (Show)

instance Exception EndpointException

instance Exception PeerMessageException

data PubSubMsg
    = Subscribe'
          { topic :: String
          }
    | Publish'
          { topic :: String
          , message :: PubNotifyMessage
          }
    | Notify'
          { topic :: String
          , message :: PubNotifyMessage
          }
    deriving (Show, Generic, Serialise)
