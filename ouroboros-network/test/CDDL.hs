{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module CDDL
where

import System.Exit (ExitCode(..))
import System.Process.ByteString.Lazy
import Control.Monad
import Control.Exception.Base (throw)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 as Char8 (putStrLn, unpack, lines)
import qualified Codec.Serialise.Class as Serialise
import Codec.CBOR.Decoding (decodeWord, decodeListLenOf, decodeBytes)
import Codec.CBOR.Encoding (encodeBytes, encodeListLen, encodeWord)
import Codec.CBOR.Term
import Codec.CBOR.Read
import Codec.CBOR.Write

import Test.QuickCheck
import Ouroboros.Network.Chain
import Ouroboros.Network.Protocol.ChainSync.Test as CS ()
import Ouroboros.Network.Protocol.ChainSync.Type as CS
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import Network.TypedProtocol.ReqResp.Type as ReqResp
import Ouroboros.Network.Protocol.ReqResp.Codec (codecReqResp)
import Ouroboros.Network.Protocol.ReqResp.Test as ReqResp ()
import Ouroboros.Network.Protocol.PingPong.Codec (codecPingPong)
import Network.TypedProtocol.PingPong.Type as PingPong
import Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetch)
import Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch
import Ouroboros.Network.Testing.ConcreteBlock

import Network.TypedProtocol.Codec

-- the concrete types used for the test
type CS = ChainSync DummyBytes DummyBytes
type RR = ReqResp DummyBytes DummyBytes
type BF = BlockFetch BlockHeader BlockBody

main :: IO ()
main = generateAndDecode "cddl" "diag2cbor.rb" "test/messages.cddl" 100

generateAndDecode :: FilePath -> FilePath -> FilePath -> Int -> IO ()
generateAndDecode cddlCmd diag2cborCmd cddlSpec rounds = do
    diags <- generateCBORDiag cddlCmd cddlSpec rounds
    forM_ diags $ \diag -> do
        Char8.putStrLn diag
        bytes <- diagToBytes diag2cborCmd diag
        decodeMsg $ decodeTopTerm bytes

generateCBORDiag :: FilePath -> FilePath -> Int -> IO [ByteString]
generateCBORDiag cddlCmd cddlSpec rounds = do
    result <- readProcessWithExitCode cddlCmd [cddlSpec, "generate", show rounds] BS.empty
    case result of
        (ExitFailure _, _, err) -> error $ Char8.unpack err
        (ExitSuccess, diags, err) -> do
            Char8.putStrLn err
            return $ Char8.lines diags

validateCBORDiag :: FilePath -> FilePath -> ByteString -> IO Bool
validateCBORDiag cddlCmd cddlSpec cbor = do
    result <- readProcessWithExitCode cddlCmd [cddlSpec, "validate", "-"] cbor
    case result of
        (ExitFailure _, _, err) -> error $ Char8.unpack err
        (ExitSuccess, _, _) -> return True

diagToBytes :: FilePath -> ByteString -> IO ByteString
diagToBytes diag2cborCmd diag = do
    result <- readProcessWithExitCode diag2cborCmd ["-"] diag
    case result of
        (ExitFailure _ , _, err) -> error $ Char8.unpack err
        (ExitSuccess, bytes, _) -> return bytes

bytesToDiag :: FilePath -> ByteString -> IO ByteString
bytesToDiag cbor2diagCmd diag = do
    result <- readProcessWithExitCode cbor2diagCmd ["-"] diag
    case result of
        (ExitFailure _ , _, err) -> error $ Char8.unpack err
        (ExitSuccess, bytes, _) -> return bytes

decodeFile :: FilePath -> IO ()
decodeFile f =
    BS.readFile f >>= decodeMsg . decodeTopTerm

data DummyBytes = DummyBytes BSI.ByteString

instance Serialise.Serialise DummyBytes where
    encode (DummyBytes b) = encodeBytes b
    decode = DummyBytes <$> decodeBytes

instance Arbitrary DummyBytes where
    arbitrary = (DummyBytes . BSI.packBytes) <$> arbitrary

type MonoCodec x = Codec x Codec.CBOR.Read.DeserialiseFailure IO ByteString

-- | Split the ByteString into the tag-word and the rest.
decodeTopTerm :: ByteString -> (Word, ByteString)
decodeTopTerm input
    = case deserialiseFromBytes (decodeListLenOf 2 >> decodeWord) input of
        Right (bs,tag) -> (tag,bs)
        Left err -> throw err

-- Decode a message. Throw an error if the message is not decodeable.
decodeMsg :: (Word, ByteString) -> IO ()
decodeMsg (tag, input) = case tag of
    0 -> tryParsers "chainSync"  chainSyncParsers
    1 -> tryParsers "reqResp"    reqRespParsers
    2 -> tryParsers "pingPong"   pingPongParsers
    3 -> tryParsers "blockFetch" blockFetchParsers
    4 -> return () -- "txSubmissionMessage" in branch
    5 -> return () -- error "muxControlMessage" in branch
    _ -> error "unkown tag"
    where
        tryParsers :: String -> [IO Bool] -> IO ()
        tryParsers name [] = error $ "parse failed : " ++ name
        tryParsers name (h:t) = h >>= \case
            True -> return ()
            False -> tryParsers name t

        runCodec
          :: IO (DecodeStep ByteString DeserialiseFailure IO (SomeMessage st))
          -> ByteString
          -> IO Bool
        runCodec cont bs = cont >>= \case
            DecodeDone _msg _rest -> error "runCodec: codec is DecodeDone"
            DecodeFail _f -> error "runCodec: codec is DecodeFail"
            DecodePartial next -> (next $ Just bs) >>= \case
                DecodePartial _ -> return False
                DecodeDone _msg rest -> case rest of
                    Nothing -> return True
                    Just _r  -> return False
                DecodeFail _f -> return False

        run :: forall ps (pr :: PeerRole) (st :: ps).
                Codec ps DeserialiseFailure IO ByteString
             -> PeerHasAgency pr st -> IO Bool
        run codec state = runCodec ((decode codec) state) input

        runCS = run (codecChainSync Serialise.encode Serialise.decode Serialise.encode
                                    Serialise.decode :: MonoCodec CS)
        chainSyncParsers = [
              runCS (ClientAgency CS.TokIdle)
            , runCS (ServerAgency (CS.TokNext TokCanAwait))
            , runCS (ClientAgency CS.TokIdle)
            , runCS (ServerAgency CS.TokIntersect)
            , runCS (ServerAgency CS.TokIntersect)
            ]

        runReqResp = run (codecReqResp :: MonoCodec RR)
        reqRespParsers = [
              runReqResp (ClientAgency ReqResp.TokIdle)
            , runReqResp (ServerAgency ReqResp.TokBusy)
            ]

        runPingPong = run (codecPingPong :: MonoCodec PingPong)
        pingPongParsers = [
              runPingPong (ClientAgency PingPong.TokIdle)
            , runPingPong (ServerAgency PingPong.TokBusy)
            ]

        runBlockFetch = run (codecBlockFetch Serialise.encode Serialise.encode Serialise.decode
                                             Serialise.decode :: MonoCodec BF)
        blockFetchParsers = [
              runBlockFetch (ClientAgency BlockFetch.TokIdle)
            , runBlockFetch (ServerAgency BlockFetch.TokBusy)
            , runBlockFetch (ServerAgency BlockFetch.TokStreaming)
            ]

data Protocol ps where
    CS :: Protocol (ChainSync BlockHeader (Point BlockHeader))
    ReqResp :: Protocol RR

data Msg where
    Msg :: Protocol ps -> PeerHasAgency pr (st :: ps) -> Message ps (st :: ps) (st' :: ps) -> Msg

instance Arbitrary Msg where
    arbitrary = oneof [
--         genProtocol CS
         genProtocol ReqResp
        ]

protocolToTag :: Protocol ps -> Word
protocolToTag p = case p of
    CS -> 0
    ReqResp -> 1

protocolToCodec :: Protocol ps -> MonoCodec ps
protocolToCodec p = case p of
    CS      -> codecChainSync Serialise.encode Serialise.decode Serialise.encode Serialise.decode
    ReqResp -> codecReqResp

genProtocol :: Arbitrary (AnyMessageAndAgency ps) => Protocol ps -> Gen Msg
genProtocol protocol = do
    (AnyMessageAndAgency agency msg) <- arbitrary
    return $ Msg protocol agency msg

encodeMsg :: Msg -> ByteString
encodeMsg (Msg protocol agency msg)
      -- Put msg into a wrapper CBOR term.
    = toLazyByteString (encodeListLen 2 <> (encodeWord $ protocolToTag protocol) <> encodeTerm body)
    where
        innerBS = encode (protocolToCodec protocol) agency msg
        -- Reparse the ByteString from the codec.
        body = case deserialiseFromBytes decodeTerm innerBS of
            Right (_,res) -> res
            Left err -> error $ "encodeMsg : internal error :" ++ show err

{-
prop_cddl_accepts :: Msg -> PropertyM IO Bool
prop_cddl_accepts msg = do



generateAndDecode :: FilePath -> FilePath -> FilePath -> Int -> IO ()
generateAndDecode cddlCmd diag2cborCmd cddlSpec rounds = do
    diags <- generateCBORDiag cddlCmd cddlSpec rounds
    forM_ diags $ \diag -> do
        Char8.putStrLn diag
        bytes <- diagToBytes diag2cborCmd diag
        decodeMsg $ decodeTopTerm bytes
-}

encodeAndValidate :: FilePath -> FilePath -> FilePath -> Msg -> IO Bool
encodeAndValidate cddlCmd cbor2diagCmd cddlSpec msg = do
    diag <- bytesToDiag cbor2diagCmd $ encodeMsg msg
    validateCBORDiag cddlCmd cddlSpec diag

