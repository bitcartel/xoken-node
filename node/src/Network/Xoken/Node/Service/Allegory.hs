{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Network.Xoken.Node.Service.Allegory where

import Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig, networkConfig)
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Env
import Arivi.P2P.PubSub.Publish as Pub
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types hiding (msgType)
import Codec.Serialise
import Conduit hiding (runResourceT)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled, mapConcurrently, mapConcurrently_, race_)
import qualified Control.Concurrent.Async.Lifted as LA (async, concurrently, mapConcurrently, wait)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Control.Error.Util as Extra
import Control.Exception
import Control.Exception
import qualified Control.Exception.Lifted as LE (try)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Writer.Lazy
import Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16 (decode, encode)
import Data.ByteString.Base64 as B64
import Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.UTF8 as BSU (toString)
import Data.Char
import Data.Default
import qualified Data.HashTable.IO as H
import Data.Hashable
import Data.IORef
import Data.Int
import Data.List
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Pool
import qualified Data.Serialize as S
import Data.Serialize
import qualified Data.Serialize as DS (decode, encode)
import qualified Data.Set as S
import Data.String (IsString, fromString)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding as E
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Data.Yaml
import qualified Database.Bolt as BT
import Database.XCQL.Protocol as Q
import qualified Network.Simple.TCP.TLS as TLS
import Network.Xoken.Address.Base58
import Network.Xoken.Block.Common
import Network.Xoken.Crypto.Hash
import Network.Xoken.Node.Data
import Network.Xoken.Node.Data.Allegory
import Network.Xoken.Node.Env
import Network.Xoken.Node.GraphDB
import Network.Xoken.Node.P2P.BlockSync
import Network.Xoken.Node.P2P.Common
import Network.Xoken.Node.P2P.Types
import Network.Xoken.Node.P2P.UnconfTxSync
import Network.Xoken.Node.Service.Address
import Network.Xoken.Node.Service.Transaction
import Network.Xoken.Util (bsToInteger, integerToBS)
import Numeric (showHex)
import StmContainers.Map as SM
import System.Logger as LG
import System.Logger.Message
import System.Random
import Text.Read
import Xoken
import qualified Xoken.NodeConfig as NC

xGetAllegoryNameBranch :: (HasXokenNodeEnv env m, MonadIO m) => String -> Bool -> m ([(OutPoint', [MerkleBranchNode'])])
xGetAllegoryNameBranch name isProducer = do
    dbe <- getDB
    lg <- getLogger
    res <- liftIO $ try $ withResource (pool $ graphDB dbe) (`BT.run` queryAllegoryNameBranch (DT.pack name) isProducer)
    case res of
        Right nb -> do
            liftIO $
                mapConcurrently
                    (\x -> do
                         let sp = DT.split (== ':') x
                         let txid = DT.unpack $ sp !! 0
                         let index = readMaybe (DT.unpack $ sp !! 1) :: Maybe Int32
                         case index of
                             Just i -> do
                                 rs <-
                                     liftIO $
                                     try $ withResource (pool $ graphDB dbe) (`BT.run` queryMerkleBranch (DT.pack txid))
                                 case rs of
                                     Right mb -> do
                                         let mnodes =
                                                 Data.List.map
                                                     (\y -> MerkleBranchNode' (DT.unpack $ _nodeValue y) (_isLeftNode y))
                                                     mb
                                         return $ (OutPoint' txid i, mnodes)
                                     Left (e :: SomeException) -> do
                                         err lg $ LG.msg $ "Error: xGetMerkleBranch: " ++ show e
                                         throw KeyValueDBLookupException
                             Nothing -> throw KeyValueDBLookupException)
                    (nb)
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "Error: xGetAllegoryNameBranch: " ++ show e
            throw KeyValueDBLookupException

xGetProducer :: (HasXokenNodeEnv env m, MonadIO m) => [Int] -> m (OutPoint', DT.Text)
xGetProducer nameArr = do
    dbe <- getDB
    lg <- getLogger
    let name = DT.pack $ L.map (\x -> chr x) (nameArr)
    res <- liftIO $ try $ withResource (pool $ graphDB dbe) (`BT.run` queryAllegoryNameScriptOp (name) True)
    case res of
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "error fetching allegory name input :" ++ show e
            throw e
        Right [] -> do
            debug lg $ LG.msg $ "[getProducer] Allegory name '" <> name <> "' not found, going one level above"
            if nameArr == []
                then do
                    err lg $ LG.msg $ show "[getProducer] Couldn't find Allegory root!"
                    throw KeyValueDBLookupException
                else xGetProducer $ init nameArr
        Right nb -> do
            debug lg $ LG.msg $ "[getProducer] Allegory producer '" <> name <> "' found"
            let sp = DT.split (== ':') $ fst (head nb)
            let txid = DT.unpack $ sp !! 0
            let index = readMaybe (DT.unpack $ sp !! 1) :: Maybe Int
            case index of
                Just i -> return $ (OutPoint' txid (fromIntegral i), (snd $ head nb))
                Nothing -> throw KeyValueDBLookupException

getOrMakeProducer' ::
       (HasXokenNodeEnv env m, MonadIO m) => Address -> [Int] -> m ((OutPoint', DT.Text), Bool, [BC.ByteString])
getOrMakeProducer' resellerAddress nameArr = do
    dbe <- getDB
    lg <- getLogger
    bp2pEnv <- getBitcoinP2P
    let name = DT.pack $ L.map (\x -> chr x) (nameArr)
    let anutxos = NC.allegoryNameUtxoSatoshis $ nodeConfig $ bp2pEnv
    res <- liftIO $ try $ withResource (pool $ graphDB dbe) (`BT.run` queryAllegoryNameScriptOp (name) True)
    case res of
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "error fetching allegory name input :" ++ show e
            throw e
        Right [] -> do
            debug lg $ LG.msg $ "allegory name not found, create recursively (1): " <> name
            (nameip, existed, interimTxns) <- getOrMakeProducer' resellerAddress (init nameArr)
            let net = NC.bitcoinNetwork $ nodeConfig bp2pEnv
                prScript = addressToScriptBS resellerAddress
                addr' =
                    case addrToString net resellerAddress of
                        Nothing -> ""
                        Just t -> DT.unpack t
                ins' =
                    L.map
                        (\(x, s) ->
                             TxIn
                                 (OutPoint (fromString $ opTxHash x) (fromIntegral $ opIndex x))
                                 (fromJust $ decodeHex s)
                                 0xFFFFFFFF)
                        [nameip]
            utxos <- getFundingUtxos addr'
            let (ins, fval) =
                    case L.filter (\y -> aoValue y >= 2000000) utxos of
                        [] -> (ins', 0)
                        (x:xs) ->
                            let op = aoOutput x
                             in ( ins' ++
                                  [ TxIn
                                        (OutPoint (fromString $ opTxHash op) (fromIntegral $ opIndex op))
                                        prScript
                                        0xFFFFFFFF
                                  ]
                                , aoValue x)
                al =
                    Allegory
                        1
                        (init nameArr)
                        (ProducerAction
                             (Index 0)
                             (ProducerOutput (Index 1) (Just $ Endpoint "XokenP2P" "someuri-1"))
                             Nothing
                             [ (ProducerExtension
                                    (ProducerOutput (Index 2) (Just $ Endpoint "XokenP2P" "someuri-2"))
                                    (last nameArr))
                             , (OwnerExtension
                                    (OwnerOutput (Index 3) (Just $ Endpoint "XokenP2P" "someuri-3"))
                                    (last nameArr))
                             ])
                opRetScript = frameOpReturn $ C.toStrict $ serialise al
                !outs = [TxOut 0 opRetScript] ++ L.map (\_ -> TxOut (fromIntegral anutxos) prScript) [1, 2, 3]
                unsignedTx = Tx 1 ins outs 0
            processUnconfTransaction unsignedTx
            handleIfAllegoryTx unsignedTx True
            inres <- liftIO $ try $ withResource (pool $ graphDB dbe) (`BT.run` queryAllegoryNameScriptOp (name) True)
            case inres of
                Left (e :: SomeException) -> do
                    err lg $ LG.msg $ "error fetching allegory name input :" ++ show e
                    throw e
                Right [] -> do
                    err lg $ LG.msg $ "allegory name still not found, recursive create must've failed (1): " <> name
                    throw KeyValueDBLookupException
                Right nb -> do
                    let sp = DT.split (== ':') $ fst (head nb)
                    let txid = DT.unpack $ sp !! 0
                    let index = readMaybe (DT.unpack $ sp !! 1) :: Maybe Int
                    case index of
                        Just i ->
                            return $
                            ( (OutPoint' txid (fromIntegral i), (snd $ head nb))
                            , False
                            , (BSL.toStrict $ A.encode $ unsignedTx) : interimTxns)
                        Nothing -> throw KeyValueDBLookupException
        Right nb -> do
            debug lg $ LG.msg $ "allegory name found! (1): " <> name
            let sp = DT.split (== ':') $ fst (head nb)
            let txid = DT.unpack $ sp !! 0
            let index = readMaybe (DT.unpack $ sp !! 1) :: Maybe Int
            case index of
                Just i -> return $ ((OutPoint' txid (fromIntegral i), (snd $ head nb)), True, [])
                Nothing -> throw KeyValueDBLookupException

xGetPartiallySignedAllegoryTx ::
       (HasXokenNodeEnv env m, MonadIO m)
    => [(OutPoint', Int)]
    -> ([Int], Bool)
    -> (String)
    -> (String)
    -> Address
    -> Int
    -> m ([BC.ByteString], BC.ByteString)
xGetPartiallySignedAllegoryTx payips (nameArr, isProducer) owner change resellerAddress paySats = do
    dbe <- getDB
    bp2pEnv <- getBitcoinP2P
    lg <- getLogger
    let conn = xCqlClientState dbe
    let net = NC.bitcoinNetwork $ nodeConfig bp2pEnv
     -- check if name (of given type) exists
    let name = DT.pack $ L.map (\x -> chr x) (nameArr)
     -- read from config file
    let anutxos = NC.allegoryNameUtxoSatoshis $ nodeConfig $ bp2pEnv
    let feeSatsCreate = NC.allegoryTxFeeSatsProducerAction $ nodeConfig $ bp2pEnv
    let feeSatsTransfer = NC.allegoryTxFeeSatsOwnerAction $ nodeConfig $ bp2pEnv
    res <- liftIO $ try $ withResource (pool $ graphDB dbe) (`BT.run` queryAllegoryNameScriptOp (name) isProducer)
    (nameip, existed, interimTxns) <-
        case res of
            Left (e :: SomeException) -> do
                err lg $ LG.msg $ "error fetching allegory name input :" ++ show e
                throw e
            Right [] -> do
                debug lg $ LG.msg $ "allegory name not found, get or make interim producers recursively : " <> name
                getOrMakeProducer' resellerAddress (init nameArr)
            Right nb -> do
                debug lg $ LG.msg $ "allegory name found! : " <> name
                let sp = DT.split (== ':') $ fst (head nb)
                let txid = DT.unpack $ sp !! 0
                let index = readMaybe (DT.unpack $ sp !! 1) :: Maybe Int32
                case index of
                    Just i -> do
                        return $ ((OutPoint' txid i, (snd $ head nb)), True, [])
                    Nothing -> do
                        debug lg $ LG.msg $ val "allegory case index of : Nothing"
                        throw KeyValueDBLookupException
    inputHash <-
        liftIO $
        traverse
            (\(w, _) -> do
                 let op = OutPoint (fromString $ opTxHash w) (fromIntegral $ opIndex w)
                 sh <- getScriptHashFromOutpoint conn (txSynchronizer bp2pEnv) lg net op 0
                 return $ (w, ) <$> sh)
            payips
    let totalEffectiveInputSats = sum $ snd $ unzip payips
    let ins =
            L.map
                (\(x, s) ->
                     TxIn
                         (OutPoint (fromString $ opTxHash x) (fromIntegral $ opIndex x))
                         (fromJust $ decodeHex s)
                         0xFFFFFFFF)
                ([nameip] ++ (catMaybes inputHash))
    let outs =
            if existed
                then if isProducer
                         then do
                             let al =
                                     Allegory
                                         1
                                         (init nameArr)
                                         (ProducerAction
                                              (Index 0)
                                              (ProducerOutput (Index 1) (Just $ Endpoint "XokenP2P" "someuri_1"))
                                              Nothing
                                              [])
                             let opRetScript = frameOpReturn $ C.toStrict $ serialise al
                             -- derive producer's Address
                             let prScript = addressToScriptBS resellerAddress
                             let payScript = addressToScriptBS resellerAddress
                             let changeSats = totalEffectiveInputSats - (paySats + feeSatsCreate)
                             [TxOut 0 opRetScript] ++
                                 (L.map
                                      (\x -> do
                                           let addr =
                                                   case stringToAddr net (DT.pack $ fst x) of
                                                       Just a -> a
                                                       Nothing -> throw InvalidOutputAddressException
                                           let script = addressToScriptBS addr
                                           TxOut (fromIntegral $ snd x) script)
                                      [(owner, (fromIntegral $ anutxos)), (change, changeSats)]) ++
                                 [TxOut ((fromIntegral paySats) :: Word64) payScript] -- the charge for the name transfer
                         else do
                             let al =
                                     Allegory
                                         1
                                         (nameArr)
                                         (OwnerAction
                                              (Index 0)
                                              (OwnerOutput (Index 1) (Just $ Endpoint "XokenP2P" "someuri_1"))
                                              [ ProxyProvider
                                                    "AllPay"
                                                    "Public"
                                                    (Endpoint "XokenP2P" "someuri_2")
                                                    (Registration "addrCommit" "utxoCommit" "signature" 876543)
                                              ])
                             let opRetScript = frameOpReturn $ C.toStrict $ serialise al
                             let payScript = addressToScriptBS resellerAddress
                             let changeSats = totalEffectiveInputSats - (paySats + feeSatsTransfer)
                             [TxOut 0 opRetScript] ++
                                 (L.map
                                      (\x -> do
                                           let addr =
                                                   case stringToAddr net (DT.pack $ fst x) of
                                                       Just a -> a
                                                       Nothing -> throw InvalidOutputAddressException
                                           let script = addressToScriptBS addr
                                           TxOut (fromIntegral $ snd x) script)
                                      [(owner, (fromIntegral $ anutxos)), (change, changeSats)]) ++
                                 [TxOut (fromIntegral anutxos) payScript] -- the charge for the name transfer
                else do
                    let al =
                            Allegory
                                1
                                (init nameArr)
                                (ProducerAction
                                     (Index 0)
                                     (ProducerOutput (Index 1) (Just $ Endpoint "XokenP2P" "someuri_1"))
                                     Nothing
                                     [ OwnerExtension
                                           (OwnerOutput (Index 2) (Just $ Endpoint "XokenP2P" "someuri_3"))
                                           (last nameArr)
                                     ])
                    let opRetScript = frameOpReturn $ C.toStrict $ serialise al
                     -- derive producer's Address
                    let prScript = addressToScriptBS resellerAddress
                    let payScript = addressToScriptBS resellerAddress
                    let changeSats = totalEffectiveInputSats - ((fromIntegral $ anutxos) + paySats + feeSatsCreate)
                    [TxOut 0 opRetScript] ++
                        [TxOut (fromIntegral anutxos) prScript] ++
                        (L.map
                             (\x -> do
                                  let addr =
                                          case stringToAddr net (DT.pack $ fst x) of
                                              Just a -> a
                                              Nothing -> throw InvalidOutputAddressException
                                  let script = addressToScriptBS addr
                                  TxOut (fromIntegral $ snd x) script)
                             [(owner, (fromIntegral $ anutxos)), (change, changeSats)]) ++
                        [TxOut ((fromIntegral paySats) :: Word64) payScript] -- the charge for the name transfer
     --
    let psatx = Tx version ins outs locktime
    return $ (L.reverse interimTxns, BSL.toStrict $ A.encode $ psatx)
  where
    version = 1
    locktime = 0

getInputsForUnconfirmedTx :: (HasXokenNodeEnv env m, MonadIO m) => OutPoint' -> m [OutPoint']
getInputsForUnconfirmedTx op = do
    dbe <- getDB
    lg <- getLogger
    bp2pEnv <- getBitcoinP2P
    let conn = xCqlClientState dbe
        (txid, index) = (opTxHash op, opIndex op)
        str = "SELECT other FROM xoken.ep_txid_outputs WHERE epoch IN (True,False) AND txid=? AND output_index=?"
        qstr = str :: Q.QueryString Q.R (DT.Text, Int32) (Identity (Set ((DT.Text, Int32), Int32, (DT.Text, Int64))))
        par = getSimpleQueryParam (DT.pack $ opTxHash op, opIndex op)
    res <- liftIO $ try $ query conn (Q.RqQuery $ Q.Query qstr par)
    case (concat . ((runIdentity . (Q.fromSet <$>)) <$>)) <$> res of
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "Error: getInputsForUnconfirmedTx: " <> show e
            throw KeyValueDBLookupException
        Right os -> do
            return $ (\((txid, index), _, _) -> OutPoint' (DT.unpack txid) index) <$> os

getUnconfirmedOutputsForAddress :: (HasXokenNodeEnv env m, MonadIO m) => String -> m [OutPoint']
getUnconfirmedOutputsForAddress addr = do
    dbe <- getDB
    lg <- getLogger
    bp2pEnv <- getBitcoinP2P
    let conn = xCqlClientState dbe
        net = NC.bitcoinNetwork $ nodeConfig bp2pEnv
        sh = convertToScriptHash net addr
        str = "SELECT output FROM xoken.ep_script_hash_outputs WHERE epoch IN (True,False) AND script_hash=?"
        qstr = str :: Q.QueryString Q.R (Identity DT.Text) (Identity (DT.Text, Int32))
        par = getSimpleQueryParam (Identity (maybe "" DT.pack sh))
    res <- liftIO $ try $ query conn (Q.RqQuery $ Q.Query qstr par)
    case res of
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "Error: getUnconfirmedOutputsForAddress: " <> show e
            throw KeyValueDBLookupException
        Right res -> do
            return $ nub $ (\(Identity op) -> OutPoint' (DT.unpack $ fst op) (snd op)) <$> res

getFundingUtxos :: (HasXokenNodeEnv env m, MonadIO m) => String -> m [AddressOutputs]
getFundingUtxos addr = do
    lg <- getLogger
    res <- xGetUTXOsAddress addr (Just 2000) Nothing
    let utxos = (\(ResultWithCursor ao _) -> ao) <$> res
    unconfOutputs <- getUnconfirmedOutputsForAddress addr
    possiblySpentInputs <- liftM concat $ sequence $ getInputsForUnconfirmedTx <$> unconfOutputs
    let fundingUtxos = L.filter (\utxo -> (aoOutput utxo `L.notElem` possiblySpentInputs)) utxos
    return fundingUtxos
