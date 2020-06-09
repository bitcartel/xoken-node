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

module Network.Xoken.Node.XokenService where

import Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig, networkConfig)
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Env
import Arivi.P2P.PubSub.Publish as Pub
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types hiding (msgType)
import Codec.Compression.GZip as GZ
import Codec.Serialise
import Conduit hiding (runResourceT)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (async)
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
import Data.Aeson
import qualified Data.ByteString.Base16 as B16 (decode, encode)
import Data.ByteString.Base64 as B64
import Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.UTF8 as BSU (toString)
import Data.Default
import Data.Hashable
import Data.Int
import Data.List
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Pool
import qualified Data.Serialize as S
import Data.Serialize
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import Data.Yaml
import qualified Database.Bolt as BT
import qualified Database.CQL.IO as Q
import Database.CQL.Protocol
import Network.Xoken.Node.Data
import Network.Xoken.Node.Data.Allegory
import Network.Xoken.Node.Data.Allegory
import Network.Xoken.Node.Env
import Network.Xoken.Node.GraphDB
import Network.Xoken.Node.P2P.BlockSync
import Network.Xoken.Node.P2P.Common
import Network.Xoken.Node.P2P.Types
import System.Logger as LG
import System.Logger.Message
import Text.Read
import Xoken

data AriviServiceException
    = KeyValueDBLookupException
    | GraphDBLookupException
    | InvalidOutputAddressException
    deriving (Show)     

getd :: (Identity DT.Text) -> DT.Text
getd (Identity a) = a

instance Exception AriviServiceException


xGetBlockHash :: (HasXokenNodeEnv env m, MonadIO m) => Network -> String -> m (Maybe BlockRecord)
xGetBlockHash net hash = do
    dbe <- getDB
    let conn = keyValDB (dbe)
        str = "SELECT block_hash, block_height, block_header from xoken.blocks_by_hash where block_hash = ?"
        qstr = str :: Q.QueryString Q.R (Identity DT.Text) (DT.Text, Int32, DT.Text)
        p = Q.defQueryParams Q.One $ Identity $ DT.pack hash
    iop <- Q.runClient conn (Q.query qstr p)
    if length iop == 0
        then return Nothing
        else do
            let (hs, ht, hdr) = iop !! 0
            case eitherDecode $ BSL.fromStrict $ DTE.encodeUtf8 hdr of
                Right bh -> do                   
                   
                    let str1 = "select txid from xoken.blockmap where blockhash = ?"
                        qstr1 = str1 :: Q.QueryString Q.R (Identity DT.Text) (Identity DT.Text)
                        p1 = Q.defQueryParams Q.One (Identity $ DT.pack hash)                          
                    iopt <- liftIO $ try $ Q.runClient conn (Q.query qstr1 p1)
                    case iopt of
                             Right (txi) -> do
                                    
                                    tx <- xGetTxHash net (DT.unpack $ getd $ txi !! 0)
                                    case tx of
                                      Just RawTxRecord {..} ->
                                          case S.decodeLazy txSerialized of
                                              Right rt ->
                                                  return $ (Just $ BlockRecord (fromIntegral ht) (DT.unpack hs) bh (TxRecord txId txBlockInfo rt))
                                              Left err -> return $ Nothing
                             Left (e :: SomeException) -> do
                                    liftIO $ print $ "Decode failed with error-: " ++ (show e) 
                                    return Nothing

                Left err -> do
                    liftIO $ print $ "Decode failed with error: " <> show err
                    return Nothing

--xGetBlocksHashes :: (HasXokenNodeEnv env m, MonadIO m) => Network -> [String] -> m ([BlockRecord])
--xGetBlocksHashes net hashes = do
--    dbe <- getDB
--    let conn = keyValDB (dbe)
--        str = "SELECT block_hash, block_height, block_header from xoken.blocks_by_hash where block_hash in ?"
--        qstr = str :: Q.QueryString Q.R (Identity [DT.Text]) (DT.Text, Int32, DT.Text)
--        p = Q.defQueryParams Q.One $ Identity $ Data.List.map (DT.pack) hashes
--    iop <- Q.runClient conn (Q.query qstr p)
--    if length iop == 0
--        then return []
--        else do
            
--            let str1 = "select txid from xoken.blockmap where blockhash in ?"
--                qstr1 = str1 :: Q.QueryString Q.R (Identity [DT.Text]) (Identity DT.Text)
--                p1 = Q.defQueryParams Q.One (Identity $ Data.List.map $ (DT.pack) hashes)                          
--            iopt <- liftIO $ try $ Q.runClient conn (Q.query qstr1 p1)
--            case traverse
--                     (\(hs, ht, hdr) ->
                       
--                          BlockRecord (fromIntegral ht) (DT.unpack hs) <$> (eitherDecode $ BSL.fromStrict $ DTE.encodeUtf8 hdr))
--                     (iop) of
--                Right x -> return x
--                Left err -> do
--                    liftIO $ print $ "decode failed for blockrecord: " <> show err
--                    return []

-- xGetBlockHeight :: (HasXokenNodeEnv env m, MonadIO m) => Network -> Int32 -> m (Maybe BlockRecord)
-- xGetBlockHeight net height = do
--     dbe <- getDB
--     let conn = keyValDB (dbe)
--         str = "SELECT block_hash, block_height, block_header from xoken.blocks_by_height where block_height = ?"
--         qstr = str :: Q.QueryString Q.R (Identity Int32) (DT.Text, Int32, DT.Text)
--         p = Q.defQueryParams Q.One $ Identity height
--     iop <- Q.runClient conn (Q.query qstr p)
--     if length iop == 0
--         then return Nothing
--         else do
--             let (hs, ht, hdr) = iop !! 0
--             case eitherDecode $ BSL.fromStrict $ DTE.encodeUtf8 hdr of
--                 Right bh -> return $ Just $ BlockRecord (fromIntegral ht) (DT.unpack hs) bh
--                 Left err -> do
--                     liftIO $ print $ "Decode failed with error: " <> show err
--                     return Nothing

-- xGetBlocksHeights :: (HasXokenNodeEnv env m, MonadIO m) => Network -> [Int32] -> m ([BlockRecord])
-- xGetBlocksHeights net heights = do
--     dbe <- getDB
--     let conn = keyValDB (dbe)
--         str = "SELECT block_hash, block_height, block_header from xoken.blocks_by_height where block_height in ?"
--         qstr = str :: Q.QueryString Q.R (Identity [Int32]) (DT.Text, Int32, DT.Text)
--         p = Q.defQueryParams Q.One $ Identity heights
--     iop <- Q.runClient conn (Q.query qstr p)
--     if length iop == 0
--         then return []
--         else do
--             case traverse
--                      (\(hs, ht, hdr) ->
--                           BlockRecord (fromIntegral ht) (DT.unpack hs) <$> (eitherDecode $ BSL.fromStrict $ DTE.encodeUtf8 hdr))
--                      (iop) of
--                 Right x -> return x
--                 Left err -> do
--                     liftIO $ print $ "decode failed for blockrecord: " <> show err
--                     return []

xGetTxHash :: (HasXokenNodeEnv env m, MonadIO m) => Network -> String -> m (Maybe RawTxRecord)
xGetTxHash net hash = do
    dbe <- getDB
    let conn = keyValDB (dbe)
        str = "SELECT tx_id, block_info, tx_serialized from xoken.transactions where tx_id = ?"
        qstr = str :: Q.QueryString Q.R (Identity DT.Text) (DT.Text, ((DT.Text, Int32), Int32), Blob)
        p = Q.defQueryParams Q.One $ Identity $ DT.pack hash
    iop <- Q.runClient conn (Q.query qstr p)
    if length iop == 0
        then return Nothing
        else do
            let (txid, ((bhash, txind), blkht), sz) = iop !! 0
            return $
                Just $
                RawTxRecord
                    (DT.unpack txid)
                    (BlockInfo' (DT.unpack bhash) (fromIntegral txind) (fromIntegral blkht))
                    (fromBlob sz)

xGetTxHashes :: (HasXokenNodeEnv env m, MonadIO m) => Network -> [String] -> m ([RawTxRecord])
xGetTxHashes net hashes = do
    dbe <- getDB
    let conn = keyValDB (dbe)
        str = "SELECT tx_id, block_info, tx_serialized from xoken.transactions where tx_id in ?"
        qstr = str :: Q.QueryString Q.R (Identity [DT.Text]) (DT.Text, ((DT.Text, Int32), Int32), Blob)
        p = Q.defQueryParams Q.One $ Identity $ Data.List.map (DT.pack) hashes
    iop <- Q.runClient conn (Q.query qstr p)
    if length iop == 0
        then return []
        else do
            return $
                Data.List.map
                    (\(txid, ((bhash, txind), blkht), sz) ->
                         RawTxRecord
                             (DT.unpack txid)
                             (BlockInfo' (DT.unpack bhash) (fromIntegral txind) (fromIntegral blkht))
                             (fromBlob sz))
                    iop

xGetOutputsAddress :: (HasXokenNodeEnv env m, HasLogger m, MonadIO m) => Network -> String -> m ([AddressOutputs])
xGetOutputsAddress net address = do
    dbe <- getDB
    lg <- getLogger
    let conn = keyValDB (dbe)
        str =
            "SELECT address,output,block_info,is_block_confirmed,is_output_spent,is_type_receive,other_address,prev_outpoint,value from xoken.address_outputs where address = ?"
        qstr =
            str :: Q.QueryString Q.R (Identity DT.Text) ( DT.Text
                                                        , (DT.Text, Int32)
                                                        , ((DT.Text, Int32), Int32)
                                                        , Bool
                                                        , Bool
                                                        , Bool
                                                        , Maybe DT.Text
                                                        , (DT.Text, Int32)
                                                        , Int64)
        p = Q.defQueryParams Q.One $ Identity $ DT.pack address
    res <- LE.try $ Q.runClient conn (Q.query qstr p)
    case res of
        Right iop -> do
            if length iop == 0
                then return []
                else do
                    return $
                        Data.List.map
                            (\(addr, (txhs, ind), ((bhash, txind), blkht), fconf, fospent, freceive, oaddr, (ptxhs, pind), val) ->
                                 AddressOutputs
                                     (DT.unpack addr)
                                     (OutPoint' (DT.unpack txhs) (fromIntegral ind))
                                     (BlockInfo' (DT.unpack bhash) (fromIntegral txind) (fromIntegral blkht))
                                     fconf
                                     fospent
                                     freceive
                                     (if isJust oaddr
                                          then DT.unpack $ fromJust oaddr
                                          else "")
                                     (OutPoint' (DT.unpack ptxhs) (fromIntegral pind))
                                     val)
                            iop
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "Error: xGetOutputsAddress: " ++ show e
            throw KeyValueDBLookupException

xGetOutputsAddresses :: (HasXokenNodeEnv env m, HasLogger m, MonadIO m) => Network -> [String] -> m ([AddressOutputs])
xGetOutputsAddresses net addresses = do
    dbe <- getDB
    lg <- getLogger
    let conn = keyValDB (dbe)
        str =
            "SELECT address,output,block_info,is_block_confirmed,is_output_spent,is_type_receive,other_address,prev_outpoint,value from xoken.address_outputs where address in ?"
        qstr =
            str :: Q.QueryString Q.R (Identity [DT.Text]) ( DT.Text
                                                          , (DT.Text, Int32)
                                                          , ((DT.Text, Int32), Int32)
                                                          , Bool
                                                          , Bool
                                                          , Bool
                                                          , Maybe DT.Text
                                                          , (DT.Text, Int32)
                                                          , Int64)
        p = Q.defQueryParams Q.One $ Identity $ Data.List.map (DT.pack) addresses
    res <- LE.try $ Q.runClient conn (Q.query qstr p)
    case res of
        Right iop -> do
            if length iop == 0
                then return []
                else do
                    return $
                        Data.List.map
                            (\(addr, (txhs, ind), ((bhash, txind), blkht), fconf, fospent, freceive, oaddr, (ptxhs, pind), val) ->
                                 AddressOutputs
                                     (DT.unpack addr)
                                     (OutPoint' (DT.unpack txhs) (fromIntegral ind))
                                     (BlockInfo' (DT.unpack bhash) (fromIntegral txind) (fromIntegral blkht))
                                     fconf
                                     fospent
                                     freceive
                                     (if isJust oaddr
                                          then DT.unpack $ fromJust oaddr
                                          else "")
                                     (OutPoint' (DT.unpack ptxhs) (fromIntegral pind))
                                     val)
                            iop
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "Error: xGetOutputsAddresses: " ++ show e
            throw KeyValueDBLookupException

xGetMerkleBranch :: (HasXokenNodeEnv env m, HasLogger m, MonadIO m) => Network -> String -> m ([MerkleBranchNode'])
xGetMerkleBranch net txid = do
    dbe <- getDB
    lg <- getLogger
    res <- liftIO $ try $ withResource (pool $ graphDB dbe) (`BT.run` queryMerkleBranch (DT.pack txid))
    case res of
        Right mb -> do
            return $ Data.List.map (\x -> MerkleBranchNode' (DT.unpack $ _nodeValue x) (_isLeftNode x)) mb
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "Error: xGetMerkleBranch: " ++ show e
            throw KeyValueDBLookupException

xGetAllegoryNameBranch ::
       (HasXokenNodeEnv env m, HasLogger m, MonadIO m) => Network -> String -> Bool -> m ([OutPoint'])
xGetAllegoryNameBranch net name isProducer = do
    dbe <- getDB
    lg <- getLogger
    res <- liftIO $ try $ withResource (pool $ graphDB dbe) (`BT.run` queryAllegoryNameBranch (DT.pack name) isProducer)
    case res of
        Right nb -> do
            return $
                Data.List.map
                    (\x -> do
                         let sp = DT.split (== ':') x
                         let txid = DT.unpack $ sp !! 0
                         let index = readMaybe (DT.unpack $ sp !! 1) :: Maybe Int
                         case index of
                             Just i -> OutPoint' txid i
                             Nothing -> throw KeyValueDBLookupException)
                    (nb)
        Left (e :: SomeException) -> do
            err lg $ LG.msg $ "Error: xGetAllegoryNameBranch: " ++ show e
            throw KeyValueDBLookupException

xGetPartiallySignedAllegoryTx ::
       (HasXokenNodeEnv env m, HasLogger m, MonadIO m)
    => Network
    -> [OutPoint']
    -> String
    -> Bool
    -> (String, Int)
    -> (String, Int)
    -> m (BC.ByteString)
xGetPartiallySignedAllegoryTx net payips name isProducer owner change = do
    dbe <- getDB
    bp2pEnv <- getBitcoinP2P
    lg <- getLogger
    alg <- getAllegory
    let conn = keyValDB (dbe)
    res <- liftIO $ try $ withResource (pool $ graphDB dbe) (`BT.run` queryAllegoryNameBranch (DT.pack name) isProducer)
    nameip <-
        case res of
            Left (e :: SomeException) -> do
                err lg $ LG.msg $ "error fetching allegory name input :" ++ show e
                throw e
            Right nb -> do
                let sp = DT.split (== ':') (last nb)
                let txid = DT.unpack $ sp !! 0
                let index = readMaybe (DT.unpack $ sp !! 1) :: Maybe Int
                case index of
                    Just i -> return $ OutPoint' txid i
                    Nothing -> throw KeyValueDBLookupException
    let ins =
            L.map (\x -> TxIn (OutPoint (read $ opTxHash x) (fromIntegral $ opIndex x)) BC.empty 0) (payips ++ [nameip])
    let outs =
            L.map
                (\x -> do
                     let addr =
                             case stringToAddr net (DT.pack $ fst x) of
                                 Just a -> a
                                 Nothing -> throw InvalidOutputAddressException
                     let script = addressToScriptBS addr
                     TxOut (fromIntegral $ snd x) script)
                [owner, change]
    let psatx = Tx version ins outs locktime
    return $ BC.empty
  where
    version = 1
    locktime = 0

xRelayTx :: (HasXokenNodeEnv env m, MonadIO m) => Network -> BC.ByteString -> m (Bool)
xRelayTx net rawTx = do
    dbe <- getDB
    bp2pEnv <- getBitcoinP2P
    lg <- getLogger
    let conn = keyValDB (dbe)
    -- broadcast Tx
    case runGetState (getConfirmedTx) (rawTx) 0 of
        Left e -> do
            err lg $ LG.msg $ "error decoding rawTx :" ++ show e
            throw ConfirmedTxParseException
        Right res -> do
            debug lg $ LG.msg $ val $ "broadcasting tx"
            case fst res of
                Just tx -> do
                    let outpoints = L.map (\x -> prevOutput x) (txIn tx)
                    tr <-
                        mapM
                            (\x -> do
                                 let txid = txHashToHex $ outPointHash $ prevOutput x
                                     str =
                                         "SELECT tx_id, block_info, tx_serialized from xoken.transactions where tx_id = ?"
                                     qstr =
                                         str :: Q.QueryString Q.R (Identity DT.Text) ( DT.Text
                                                                                     , ((DT.Text, Int32), Int32)
                                                                                     , Blob)
                                     p = Q.defQueryParams Q.One $ Identity $ (txid)
                                 iop <- Q.runClient conn (Q.query qstr p)
                                 if length iop == 0
                                     then do
                                         debug lg $ LG.msg $ "not found" ++ show txid
                                         return Nothing
                                     else do
                                         let (txid, _, sz) = iop !! 0
                                         case runGetLazy (getConfirmedTx) (fromBlob sz) of
                                             Left e -> do
                                                 debug lg $ LG.msg (encodeHex $ BSL.toStrict $ fromBlob sz)
                                                 return Nothing
                                             Right (txd) -> do
                                                 case txd of
                                                     Nothing -> return Nothing
                                                     Just txn -> do
                                                         let cout =
                                                                 (txOut txn) !!
                                                                 fromIntegral (outPointIndex $ prevOutput x)
                                                         case (decodeOutputBS $ scriptOutput cout) of
                                                             Right (so) -> do
                                                                 return $ Just (so, outValue cout, prevOutput x)
                                                             Left (e) -> do
                                                                 err lg $ LG.msg $ "error decoding rawTx :" ++ show e
                                                                 return Nothing)
                            (txIn tx)
                    -- if verifyStdTx net tx $ catMaybes tr
                    --     then do
                    allPeers <- liftIO $ readTVarIO (bitcoinPeers bp2pEnv)
                    let !connPeers = L.filter (\x -> bpConnected (snd x)) (M.toList allPeers)
                    debug lg $ LG.msg $ val $ "transaction verified - broadcasting tx"
                    mapM_ (\(_, peer) -> do sendRequestMessages peer (MTx (fromJust $ fst res))) connPeers
                    return True
                        -- else do
                        --     debug lg $ LG.msg $ val $ "transaction invalid"
                        --     let op_return = head (txOut tx)
                        --     let hexstr = B16.encode (scriptOutput op_return)
                        --     if "006a0f416c6c65676f72792f416c6c506179" `isPrefixOf` (BC.unpack hexstr)
                        --         then do
                        --             liftIO $ print (hexstr)
                        --             case decodeOutputScript $ scriptOutput op_return of
                        --                 Right (script) -> do
                        --                     liftIO $ print (script)
                        --                     case last $ scriptOps script of
                        --                         (OP_PUSHDATA payload _) -> do
                        --                             case (deserialiseOrFail $ C.fromStrict payload) of
                        --                                 Right (allegory) -> do
                        --                                     liftIO $ print (allegory)
                        --                                     ores <-
                        --                                         liftIO $
                        --                                         try $
                        --                                         withResource
                        --                                             (pool $ graphDB dbe)
                        --                                             (`BT.run` updateAllegoryStateTrees tx allegory)
                        --                                     case ores of
                        --                                         Right () -> return True
                        --                                         Left (SomeException e) -> return $ False
                        --                                 Left (e :: DeserialiseFailure) -> do
                        --                                     err lg $
                        --                                         LG.msg $
                        --                                         "error deserialising OP_RETURN CBOR data" ++ show e
                        --                                     return False
                        --                 Left (e) -> do
                        --                     err lg $ LG.msg $ "error decoding op_return data:" ++ show e
                        --                     return False
                        --         else do
                        --             err lg $ LG.msg $ val $ "error: OP_RETURN prefix mismatch"
                        --             return False
                Nothing -> do
                    err lg $ LG.msg $ val $ "error decoding rawTx (2)"
                    return $ False

goGetResource :: (HasXokenNodeEnv env m, MonadIO m) => RPCMessage -> Network -> m (RPCMessage)
goGetResource msg net = do
    dbe <- getDB
    let grdb = graphDB (dbe)
    case rqMethod msg of
        "HASH->BLOCK" -> do
            case rqParams msg of
                Just (GetBlockByHash hs) -> do
                    blk <- xGetBlockHash net (hs)
                    case blk of
                        Just b -> return $ RPCResponse 200 Nothing $ Just $ RespBlockByHash b
                        Nothing -> return $ RPCResponse 404 (Just INVALID_REQUEST) Nothing
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        -- "[HASH]->[BLOCK]" -> do
        --     case rqParams msg of
        --         Just (GetBlocksByHashes hashes) -> do
        --             blks <- xGetBlocksHashes net hashes
        --             return $ RPCResponse 200 Nothing $ Just $ RespBlocksByHashes blks
        --         _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        -- "HEIGHT->BLOCK" -> do
        --     case rqParams msg of
        --         Just (GetBlockByHeight ht) -> do
        --             blk <- xGetBlockHeight net (fromIntegral ht)
        --             case blk of
        --                 Just b -> do
        --                     return $ RPCResponse 200 Nothing $ Just $ RespBlockByHash b
        --                 Nothing -> return $ RPCResponse 404 (Just INVALID_REQUEST) Nothing
        --         _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        -- "[HEIGHT]->[BLOCK]" -> do
        --     case rqParams msg of
        --         Just (GetBlocksByHeight hts) -> do
        --             blks <- xGetBlocksHeights net $ Data.List.map (fromIntegral) hts
        --             return $ RPCResponse 200 Nothing $ Just $ RespBlocksByHashes blks
        --         _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "TXID->RAWTX" -> do
            case rqParams msg of
                Just (GetRawTransactionByTxID hs) -> do
                    tx <- xGetTxHash net (hs)
                    case tx of
                        Just t -> return $ RPCResponse 200 Nothing $ Just $ RespRawTransactionByTxID t
                        Nothing -> return $ RPCResponse 404 (Just INVALID_REQUEST) Nothing
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "TXID->TX" -> do
            case rqParams msg of
                Just (GetTransactionByTxID hs) -> do
                    tx <- xGetTxHash net (hs)
                    case tx of
                        Just RawTxRecord {..} ->
                            case S.decodeLazy txSerialized of
                                Right rt ->
                                    return $
                                    RPCResponse 200 Nothing $
                                    Just $ RespTransactionByTxID (TxRecord txId txBlockInfo rt)
                                Left err -> return $ RPCResponse 400 (Just INTERNAL_ERROR) Nothing
                        Nothing -> return $ RPCResponse 404 (Just INVALID_REQUEST) Nothing
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "[TXID]->[RAWTX]" -> do
            case rqParams msg of
                Just (GetRawTransactionsByTxIDs hashes) -> do
                    txs <- xGetTxHashes net hashes
                    return $ RPCResponse 200 Nothing $ Just $ RespRawTransactionsByTxIDs txs
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "[TXID]->[TX]" -> do
            case rqParams msg of
                Just (GetTransactionsByTxIDs hashes) -> do
                    txs <- xGetTxHashes net hashes
                    let rawTxs =
                            (\RawTxRecord {..} ->
                                 (TxRecord txId txBlockInfo) <$> (Extra.hush $ S.decodeLazy txSerialized)) <$>
                            txs
                    return $ RPCResponse 200 Nothing $ Just $ RespTransactionsByTxIDs $ catMaybes rawTxs
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "ADDR->[OUTPUT]" -> do
            case rqParams msg of
                Just (GetOutputsByAddress addr) -> do
                    ops <- xGetOutputsAddress net (addr)
                    return $ RPCResponse 200 Nothing $ Just $ RespOutputsByAddress ops
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "[ADDR]->[OUTPUT]" -> do
            case rqParams msg of
                Just (GetOutputsByAddresses addrs) -> do
                    ops <- xGetOutputsAddresses net addrs
                    return $ RPCResponse 200 Nothing $ Just $ RespOutputsByAddresses ops
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "TXID->[MNODE]" -> do
            case rqParams msg of
                Just (GetMerkleBranchByTxID txid) -> do
                    ops <- xGetMerkleBranch net txid
                    return $ RPCResponse 200 Nothing $ Just $ RespMerkleBranchByTxID ops
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "NAME->[OUTPOINT]" -> do
            case rqParams msg of
                Just (GetAllegoryNameBranch name isProducer) -> do
                    ops <- xGetAllegoryNameBranch net name isProducer
                    return $ RPCResponse 200 Nothing $ Just $ RespAllegoryNameBranch ops
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "RELAY_TX" -> do
            case rqParams msg of
                Just (RelayTx tx) -> do
                    ops <- xRelayTx net tx
                    return $ RPCResponse 200 Nothing $ Just $ RespRelayTx ops
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "PS_ALLEGORY_TX" -> do
            case rqParams msg of
                Just (GetPartiallySignedAllegoryTx payips name isProducer owner change) -> do
                    ops <- xGetPartiallySignedAllegoryTx net payips name isProducer owner change
                    return $ RPCResponse 200 Nothing $ Just $ RespPartiallySignedAllegoryTx ops
                _____ -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        _____ ->
            return $ RPCResponse 400 (Just INVALID_METHOD) Nothing
