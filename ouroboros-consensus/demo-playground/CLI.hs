module CLI (
    CLI(..)
  , TopologyInfo(..)
  , Command(..)
  , Protocol(..)
  , USSArgs(..)
  , fromProtocol
  , parseCLI
  -- * Handy re-exports
  , execParser
  , info
  , (<**>)
  , helper
  , fullDesc
  , progDesc
  ) where

import           Data.Either (either)
import           Data.Foldable (asum)
import           Data.Semigroup ((<>))
import           Data.Maybe (fromMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           Options.Applicative

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Update
import           Ouroboros.Consensus.Util

import           Mock.TxSubmission (command', parseMockTx)
import           Topology (TopologyInfo (..))

import qualified Cardano.BM.Data.Severity as Monitoring

import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Common as Chain
import qualified Cardano.Chain.Slotting as Chain

import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

data CLI = CLI {
    systemStart  :: SystemStart
  , slotDuration :: SlotLength
  , loggerConfig :: Maybe FilePath
  , loggerMinSev :: Monitoring.Severity
  , command      :: Command
  }

data Command =
    SimpleNode       TopologyInfo Protocol
  | TxSubmitter      TopologyInfo Mock.Tx
  | ProtoProposer    TopologyInfo USSArgs
  | SoftProposer     TopologyInfo USSArgs
  | Voter            TopologyInfo USSArgs

data Protocol =
    BFT
  | Praos
  | MockPBFT
  | RealPBFT

fromProtocol :: Protocol -> IO (Some DemoProtocol)
fromProtocol BFT =
    return $ Some $ DemoBFT defaultSecurityParam
fromProtocol Praos =
    return $ Some $ DemoPraos defaultDemoPraosParams
fromProtocol MockPBFT =
    return $ Some $ DemoMockPBFT (defaultDemoPBftParams genesisConfig)
  where
    -- TODO: This is nasty
    genesisConfig = error "genesis config not needed when using mock ledger"
fromProtocol RealPBFT = do
    return $ Some $ DemoRealPBFT (defaultDemoPBftParams genesisConfig)
  where
    genesisConfig = Dummy.dummyConfig

parseCLI :: Parser CLI
parseCLI = CLI
    <$> parseSystemStart
    <*> parseSlotDuration
    <*> parseLoggerConfig
    <*> parseMinSev
    <*> parseCommand

parseMinSev :: Parser Monitoring.Severity
parseMinSev = (fromMaybe Monitoring.Debug <$>) $ optional $ asum
  [ flag' Monitoring.Debug     (long "minsev-debug")
  , flag' Monitoring.Info      (long "minsev-info")
  , flag' Monitoring.Notice    (long "minsev-notice")
  , flag' Monitoring.Warning   (long "minsev-warning")
  , flag' Monitoring.Error     (long "minsev-error")
  , flag' Monitoring.Critical  (long "minsev-critical")
  , flag' Monitoring.Alert     (long "minsev-alert")
  , flag' Monitoring.Emergency (long "minsev-emergency")
  ]

parseSystemStart :: Parser SystemStart
parseSystemStart = option (SystemStart <$> auto) $ mconcat [
      long "system-start"
    , help "The start time of the system (e.g. \"2018-12-10 15:58:06\""
    ]

parseSlotDuration :: Parser SlotLength
parseSlotDuration = option (mkSlotLength <$> auto) $ mconcat [
      long "slot-duration"
    , value (mkSlotLength 5)
    , help "The slot duration (seconds)"
    ]
  where
    mkSlotLength :: Integer -> SlotLength
    mkSlotLength = slotLengthFromMillisec . (* 1000)

parseLoggerConfig :: Parser (Maybe FilePath)
parseLoggerConfig = optional $ strOption $
  long "logger-config" <>
  metavar "FILEPATH"   <>
  help "Path to the logger config file."

parseProtocol :: Parser Protocol
parseProtocol = asum [
      flag' BFT $ mconcat [
          long "bft"
        , help "Use the BFT consensus algorithm"
        ]
    , flag' Praos $ mconcat [
          long "praos"
        , help "Use the Praos consensus algorithm"
        ]
    , flag' MockPBFT $ mconcat [
          long "mock-pbft"
        , help "Use the Permissive BFT consensus algorithm using a mock ledger"
        ]
    , flag' RealPBFT $ mconcat [
          long "real-pbft"
        , help "Use the Permissive BFT consensus algorithm using the real ledger"
        ]
    ]

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
    command' "node" "Run a node." $
      SimpleNode <$> parseTopologyInfo <*> parseProtocol
  , command' "submit" "Submit a transaction." $
      TxSubmitter <$> parseTopologyInfo <*> parseMockTx
  , command' "propose-protocol" "Submit a protocol-only update proposal." $
      ProtoProposer <$> parseTopologyInfo <*> (ProposeProtocol <$> parseProposalBodyProto)
  , command' "propose-software" "Submit a software-only update proposal." $
      SoftProposer <$> parseTopologyInfo <*> (ProposeSoftware <$> parseProposalBodySoft)
  , command' "vote" "Submit a vote." $
      Voter <$> parseTopologyInfo <*> parseVote
  ]

parseProposalBodyProto :: Parser MProposalBody
parseProposalBodyProto = MProposalBody
    <$> (Just <$> parseProtocolVersion)
    <*> (Just <$> parseProtocolParametersUpdate)
    <*> pure Nothing
    <*> pure mempty

parseProtocolVersion :: Parser Update.ProtocolVersion
parseProtocolVersion = Update.ProtocolVersion
    <$> option auto (long "major")
    <*> option auto (long "minor")
    <*> option auto (long "alt")

parseProtocolParametersUpdate :: Parser Update.ProtocolParametersUpdate
parseProtocolParametersUpdate = Update.ProtocolParametersUpdate
    <$> optional (option auto (
            long "script-version"))
    <*> optional (option (undefined) ( -- XXX
            long "slot-duration"))
    <*> optional (option auto (
            long "max-block-size"))
    <*> optional (option auto (
            long "max-header-size"))
    <*> optional (option auto (
            long "max-tx-size"))
    <*> optional (option auto (
            long "max-proposal-size"))
    <*> optional (option (Chain.LovelacePortion <$> auto) (
            long "mpc-thd"))
    <*> optional (option (Chain.LovelacePortion <$> auto) (
            long "heavy-del-thd"))
    <*> optional (option (Chain.LovelacePortion <$> auto) (
            long "update-vote-thd"))
    <*> optional (option (Chain.LovelacePortion <$> auto) (
            long "update-proposal-thd"))
    <*> optional (option (Chain.FlatSlotId <$> auto) (
            long "update-implicit"))
    <*> optional parseSoftforkRule
    <*> optional parseTxFeePolicy
    <*> optional (option (Chain.EpochIndex <$> auto) (
            long "unlock-stake-epoch"))

parseTxFeePolicy :: Parser Chain.TxFeePolicy
parseTxFeePolicy = (Chain.TxFeePolicyTxSizeLinear <$>) $ Chain.TxSizeLinear
    <$> (option (mkLovelace <$> auto) (long "txfee-policy-a"))
    <*> (option (mkLovelace <$> auto) (long "txfee-policy-b"))
  where mkLovelace :: Word64 -> Chain.Lovelace
        mkLovelace = either (\err -> error (show err)) id . Chain.mkLovelace

parseSoftforkRule :: Parser Update.SoftforkRule
parseSoftforkRule = Update.SoftforkRule
    <$> option (Chain.LovelacePortion <$> auto) (long "init-thd")
    <*> option (Chain.LovelacePortion <$> auto) (long "min-thd")
    <*> option (Chain.LovelacePortion <$> auto) (long "thd-decrement")

parseProposalBodySoft :: Parser MProposalBody
parseProposalBodySoft = MProposalBody
    <$> pure Nothing
    <*> pure Nothing
    <*> (Just <$> parseSoftwareVersion)
    <*> (interpretPairs <$> many parseSystemTagAppHash)
  where
    parseSoftwareVersion :: Parser Update.SoftwareVersion
    parseSoftwareVersion = Update.SoftwareVersion
      <$> option (Update.ApplicationName <$> auto) (long "app-name")
      <*> option (auto) (long "version")
    parseSystemTagAppHash :: Parser (Update.SystemTag, Update.InstallerHash)
    parseSystemTagAppHash = (,) <$> parseSystemTag <*> parseInstallerHash
    parseSystemTag        = option (Update.SystemTag     <$> auto) (long "system-tag")
    parseInstallerHash    = option (Update.InstallerHash <$> auto) (long "installer-hash")
    interpretPairs :: [(Update.SystemTag, Update.InstallerHash)]
                   -> Map Update.SystemTag Update.InstallerHash
    interpretPairs = Map.fromList

parseVote :: Parser USSArgs
parseVote = SubmitVote
  <$> parseUpId
  <*> asum [ flag' True $ mconcat [
               long "accept"
               , help "Vote for the proposal"
               ]
           , flag' False $ mconcat [
               long "reject"
               , help "Vote against the proposal"
               ]]

parseUpId :: Parser Update.UpId
parseUpId = option (decodeHash <$> auto)
            (long "proposal-id")
  where decodeHash = either (\err -> error (show err)) id . Crypto.decodeHash

parseNodeId :: Parser NodeId
parseNodeId =
    option (fmap CoreId auto) (
            long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help "The ID for this node"
    )

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> short 't'
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
    )

parseTopologyInfo :: Parser TopologyInfo
parseTopologyInfo = TopologyInfo <$> parseNodeId <*> parseTopologyFile
