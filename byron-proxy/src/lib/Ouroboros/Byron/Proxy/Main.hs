{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Byron.Proxy.Main where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue,
                                       writeTBQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar)
import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (forM, void)
import Control.Monad.Trans.Class (lift)
import Control.Lens ((^.))
import Control.Tracer (Tracer, traceWith)
import Data.Conduit (ConduitT, (.|), await, mapOutput, runConduit, yield)
import Data.List (maximumBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..), tagWith, untag)
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Text (Builder)
import Numeric.Natural (Natural)

import Cardano.BM.Data.Severity (Severity (..))

import Pos.Binary.Class (decodeFull)
import Pos.Chain.Block (Block, BlockHeader, HeaderHash, MainBlockHeader,
                        getBlockHeader, headerHash)
import Pos.Chain.Delegation (ProxySKHeavy)
import Pos.Chain.Ssc (MCCommitment (..), MCOpening (..), MCShares (..),
                      MCVssCertificate (..), getCertId)
import Pos.Chain.Txp (TxAux (..), TxId, TxMsgContents (..))
import Pos.Chain.Update (BlockVersionData, UpdateProposal (..), UpdateVote (..),
                         UpId, VoteId)
import Pos.Communication (NodeId)
import Pos.Core (HasDifficulty(difficultyL), StakeholderId, addressHash,
                 getEpochOrSlot)
import Pos.Core.Chrono (NewestFirst (..), OldestFirst (..))
import Pos.Crypto (hash)
import Pos.DB.Class (Serialized (..), SerializedBlock)
import Pos.DB.Block (GetHeadersFromManyToError (..), GetHashesRangeError (..))
import Pos.Diffusion.Full (FullDiffusionConfiguration (..), diffusionLayerFull)
import Pos.Infra.Diffusion.Types
-- An ancient relic. Needed for the network configuration type.
import Pos.Infra.DHT.Real.Param (KademliaParams)
import Pos.Infra.Network.Types (NetworkConfig (..))
import Pos.Logic.Types hiding (streamBlocks)
import qualified Pos.Logic.Types as Logic

import Ouroboros.Byron.Proxy.DB (DB)
import qualified Ouroboros.Byron.Proxy.DB as DB
import Ouroboros.Byron.Proxy.Pool (Pool, withPool)
import qualified Ouroboros.Byron.Proxy.Pool as Pool (insert, lookup)
import Ouroboros.Storage.ImmutableDB.API (SlotNo (..))

-- | Definitions required in order to run the Byron proxy.
data ByronProxyConfig = ByronProxyConfig
  { -- TODO see if we can't derive the block version data from the database.
    bpcAdoptedBVData     :: !BlockVersionData
  , bpcNetworkConfig     :: !(NetworkConfig KademliaParams)
  , bpcDiffusionConfig   :: !FullDiffusionConfiguration
    -- | How long rounds in the pools last, in microseconds. Data in the pools
    -- will live for at least this long, and at most 2 times this long.
  , bpcPoolRoundInterval :: !Natural
    -- | Size of the send queue. Sending atomic (non-block data) to Byron
    -- will block if this queue is full.
  , bpcSendQueueSize     :: !Natural
    -- | Size of the recv queue.
    -- TODO should probably let it be unlimited, since there is no backpressure
    -- in the Byron diffusion layer anyway, so failing to clear this queue
    -- will still cause a memory leak.
  , bpcRecvQueueSize     :: !Natural
  }

-- | Interface presented by the Byron proxy.
data ByronProxy = ByronProxy
  { -- | A transaction which gives the current 'BestTip'.
    -- These are header announcements from the Byron cluster. They don't
    -- come in through a queue because there's no point in dealing with an
    -- earlier announcement when a later one is available.
    bestTip       :: STM (Maybe (BestTip BlockHeader))
    -- | Attempt to download the chain at a given header from a given peer.
    -- Those data can be taken from 'bestTip', but of course may no longer be
    -- correct at the time of the call.
    --
    -- TODO deal with the `Maybe t` in this type. Should it be there? It's
    -- used to indicate whether streaming is available, for fallback to
    -- batching.
  , downloadChain :: forall t .
                     NodeId
                  -> HeaderHash   -- of tip to request
                  -> [HeaderHash] -- of checkpoints
                  -> StreamBlocks Block IO t
                  -> IO (Maybe t)
    -- | Make Byron peers aware of this chain. It's expected that they will
    -- request it, which will be served by some database, so the blocks for
    -- this chain should be in it.
  , announceChain :: MainBlockHeader -> IO ()
    -- | Take the next atom from the Byron network (non-block data).
  , recvAtom      :: STM Atom
    -- | Send an atom to the Byron network. It's in STM because the send is
    -- performed asynchronously.
  , sendAtom      :: Atom -> STM ()
  }

-- | Make a logic layer `KeyVal` from a `Pool`. A `Tagged` is thrown on
-- because that's what the logic layer needs on all keys.
taggedKeyValFromPool
  :: ( Ord k )
  => Proxy tag
  -> (v -> k)
  -- ^ Get the key from the value.
  -> (v -> IO ())
  -- ^ Handle incoming data of this type.
  -> Pool k v
  -> KeyVal (Tagged tag k) v IO
taggedKeyValFromPool ptag keyFromValue process pool = KeyVal
  { -- This is needed by the old relay system, so it can make the INV
    -- announcements after a DATA messages has been processed.
    toKey = pure . tagWith ptag . keyFromValue
    -- Handle an INV: True if we don't have it, False if we do.
  , handleInv = \k -> fmap (maybe True (const False)) $ atomically $
      Pool.lookup (untag k) pool
    -- Handle a REQ: Nothing if we don't have it, Just if we do.
  , handleReq = \k -> atomically $ Pool.lookup (untag k) pool
    -- Handle a DATA: put it into the pool, process it, and give False to mean it
    -- should _not_ be relayed. For this Byron proxy, we'll never relay data
    -- received from Byron to another Byron node.
  , handleData = \v -> do
      atomically $ Pool.insert (keyFromValue v) v pool
      process v
      pure False
  }

type TxPool = Pool TxId TxMsgContents
type UpProposalPool = Pool UpId (UpdateProposal, [UpdateVote])
type UpVotePool = Pool VoteId UpdateVote
type SscCommitmentPool = Pool StakeholderId MCCommitment
type SscOpeningPool = Pool StakeholderId MCOpening
type SscSharesPool = Pool StakeholderId MCShares
type SscVssCertPool = Pool StakeholderId MCVssCertificate

-- | Atoms are data which are not blocks.
data Atom where
  Transaction    :: TxMsgContents -> Atom
  UpdateProposal :: (UpdateProposal, [UpdateVote]) -> Atom
  UpdateVote     :: UpdateVote -> Atom
  Commitment     :: MCCommitment -> Atom
  Opening        :: MCOpening -> Atom
  Shares         :: MCShares -> Atom
  VssCertificate :: MCVssCertificate -> Atom
  Delegation     :: ProxySKHeavy -> Atom

deriving instance Show Atom

-- To get atoms from Shelley to Byron we put them into the pool and then
-- send them using the diffusion layer.
--
-- To get them from Byron to Shelley we use the relay mechanism built in to
-- the diffusion layer: it will put the thing into the relevant pool, then
-- make and deposit an `Atom` into a queue.

sendAtomToByron :: Diffusion IO -> Atom -> IO ()
sendAtomToByron diffusion atom = case atom of

  Transaction tx -> void $ sendTx diffusion (getTxMsgContents tx)

  UpdateProposal (up, uvs) -> sendUpdateProposal diffusion (hash up) up uvs
  UpdateVote uv            -> sendVote diffusion uv

  Opening (MCOpening sid opening)      -> sendSscOpening diffusion sid opening
  Shares (MCShares sid shares)         -> sendSscShares diffusion sid shares
  VssCertificate (MCVssCertificate vc) -> sendSscCert diffusion (getCertId vc) vc
  Commitment (MCCommitment commitment) -> sendSscCommitment diffusion sid commitment
    where
    (pk, _, _) = commitment
    sid = addressHash pk

  Delegation psk -> sendPskHeavy diffusion psk

-- | Information about the best tip from the Byron network.
data BestTip tip = BestTip
  { -- | This tip ...
    btTip :: !tip
    -- | ... was announced by these peers.
  , btPeers  :: !(NonEmpty NodeId)
  }

deriving instance Show tip => Show (BestTip tip)
deriving instance Eq tip => Eq (BestTip tip)

instance Functor BestTip where
  fmap f bt = bt { btTip = f (btTip bt) }

data HeaderComparison where
  -- | Same as in the very same header.
  Same      :: HeaderComparison
  -- | Better as in strictly longer (more difficult).
  Better    :: HeaderComparison
  -- | Better as in shorter or equal length but not equal header.
  NotBetter :: HeaderComparison

-- | Equality is done via the header hash. Adjective refers to the first
-- in relation to the second: first is `Better` than the second.
compareHeaders
  :: BlockHeader
  -> BlockHeader
  -> HeaderComparison
compareHeaders bhl bhr = case headerHash bhl == headerHash bhr of
  True  -> Same
  False -> case (bhl ^. difficultyL) `compare` (bhr ^. difficultyL) of
    GT -> Better
    _  -> NotBetter

-- | Does not keep duplicates out of the set of peers, which is fine given how
-- it's used: we expect a better header to come in later and throw away the
-- previous list. Also we don't expect a peer to announce the same header
-- twice.
updateBestTip :: NodeId -> BlockHeader -> BestTip BlockHeader -> BestTip BlockHeader
updateBestTip peer header bt = case compareHeaders header (btTip bt) of
  NotBetter -> bt
  Same -> bt { btPeers = NE.cons peer (btPeers bt) }
  Better -> BestTip { btTip = header, btPeers = peer NE.:| [] }

updateBestTipMaybe :: NodeId -> BlockHeader -> Maybe (BestTip BlockHeader) -> BestTip BlockHeader
updateBestTipMaybe peer header = maybe bt (updateBestTip peer header)
  where
  bt = BestTip { btTip = header, btPeers = peer NE.:| [] }

bbsStreamBlocks
  :: DB IO
  -> (forall a . SlotNo -> Text -> IO a)
  -- ^ If decoding fails.
  -> HeaderHash
  -> (ConduitT () Block IO () -> IO t)
  -> IO t
bbsStreamBlocks db onErr hh k = bracket (DB.readFrom db (DB.FromHash hh)) DB.closeIterator $ \iter ->
  k (DB.conduitFromIterator (DB.iterator iter) .| decode)
  where
  decode :: ConduitT DB.DBRead Block IO ()
  decode = do
    mRead <- await
    case mRead of
      Nothing -> pure ()
      Just (DB.ReadEBB slot _ bytes) -> case decodeFull bytes of
        Left err -> lift $ onErr slot err
        Right (Right blk :: Block) -> lift $ onErr slot "block where EBB expected"
        Right (Left ebb  :: Block) -> yield (Left ebb)  >> decode
      Just (DB.ReadBlock slot bytes) -> case decodeFull bytes of
        Left err -> lift $ onErr slot err
        Right (Left ebb  :: Block) -> lift $ onErr slot "EBB where block expected"
        Right (Right blk :: Block) -> yield (Right blk) >> decode

bbsGetSerializedBlock
  :: DB IO
  -> HeaderHash
  -> IO (Maybe SerializedBlock)
bbsGetSerializedBlock db hh = bracket (DB.readFrom db (DB.FromHash hh)) DB.closeIterator $ \iter ->
  let conduit = mapOutput DB.dbBytes (DB.conduitFromIterator (DB.iterator iter)) .| await
  in  (fmap . fmap) Serialized (runConduit conduit)

bbsGetBlockHeader
  :: DB IO
  -> (forall a . SlotNo -> Text -> IO a)
  -> HeaderHash
  -> IO (Maybe BlockHeader)
bbsGetBlockHeader db onErr hh = bbsStreamBlocks db onErr hh $ \conduit ->
  runConduit (conduit .| consumeOne)
  where
  consumeOne :: ConduitT Block x IO (Maybe BlockHeader)
  consumeOne = (fmap . fmap) Pos.Chain.Block.getBlockHeader await

-- TODO we're supposed to give 'Either GetHashesRangeError' but let's
-- fill that in later at the use site.
--
-- Runs the block stream conduit from the first hash until either
-- - the second hash is encountered
-- - the optional limit, or max bound of Word, iterations is reached
-- - the stream ends before either of these
--
-- The resulting list includes both endpoints.
bbsGetHashesRange
  :: DB IO
  -> (forall a . SlotNo -> Text -> IO a)
  -> Maybe Word
  -> HeaderHash
  -> HeaderHash
  -> IO (Maybe (OldestFirst NonEmpty HeaderHash))
bbsGetHashesRange db onErr mLimit from to = bbsStreamBlocks db onErr from $ \conduit ->
  let producer :: ConduitT () HeaderHash IO ()
      producer = mapOutput headerHash conduit
      consumer :: ConduitT HeaderHash x IO (Maybe (OldestFirst NonEmpty HeaderHash))
      consumer = goConsumer (fromMaybe maxBound mLimit) []
      goConsumer :: Word
                 -> [HeaderHash]
                 -> ConduitT HeaderHash x IO (Maybe (OldestFirst NonEmpty HeaderHash))
      goConsumer 0 _   = pure Nothing
      goConsumer n acc = do
        next <- await
        case next of
          Nothing -> pure Nothing
          Just it -> if it == to
                     then pure (Just (OldestFirst (NE.reverse (it NE.:| acc))))
                     else goConsumer (n-1) (it : acc)

  in  runConduit (producer .| consumer)

-- Find the first checkpoint that's in the database and then stream from
-- there.
-- 
-- we're supposed to give 'Either GetHeadersFromManyToError' but will fill that
-- in later at the use site.
--
-- How is it done in byron full logic layer? For every checkpoint given, it
-- will check whether it's in the database, filtering out those which are not.
-- Then it takes the newest checkpoint that's in the database, and loads
-- from there to the endpoint (Maybe HeaderHash or the current tip if it's
-- Nothing).
-- We'll do the same thing, even though it's probably not ideal.
--
-- One difference: we demand a tip (no 'Maybe HeaderHash'). We'll fill that
-- in at the logic layer using getTip.
--
-- The checkpoint itself is not included inthe result. That's how cardano-sl
-- does it, so it's the de facto contract.
bbsGetBlockHeaders
  :: DB IO
  -> (forall a . SlotNo -> Text -> IO a)
  -> Maybe Word
  -> NonEmpty HeaderHash
  -> Maybe HeaderHash -- ^ Optional endpoint.
  -> IO (Maybe (NewestFirst NonEmpty BlockHeader))
bbsGetBlockHeaders db onErr mLimit checkpoints mTip = do
  knownCheckpoints <- fmap catMaybes $ forM (NE.toList checkpoints) (bbsGetBlockHeader db onErr)
  let newestCheckpoint = maximumBy (comparing getEpochOrSlot) knownCheckpoints
  case knownCheckpoints of
    []    -> pure Nothing
    -- Now we know `newestCheckpoints` is not _|_ (maximumBy is partial).
    _ : _ -> bbsStreamBlocks db onErr (headerHash newestCheckpoint) $ \producer ->
      runConduit $
           mapOutput Pos.Chain.Block.getBlockHeader producer
        .| consumer 0 (fromMaybe maxBound mLimit) []
  where
  consumer :: Word -- Limit
           -> Word -- Counted so far
           -> [BlockHeader]
           -> ConduitT BlockHeader x IO (Maybe (NewestFirst NonEmpty BlockHeader))
  consumer limit 0 acc = do
    -- Drop the first one.
    _ <- await
    consumer limit 1 acc
  consumer limit n acc =
    if n >= limit
    then pure Nothing
    else do
      next <- await
      case next of
        -- We give Just if we eventually reach the tip.
        Nothing        -> pure Nothing
        Just blkHeader ->
          if maybe False ((==) (headerHash blkHeader)) mTip
          then pure $ Just $ NewestFirst $ blkHeader NE.:| acc
          else consumer limit (n+1) (blkHeader : acc)

-- | See Logic.getLcaMainChain
--
-- Although it's assumed the input list is oldest-first, it's _not_ necessarily
-- a contiguous chain. It _is_ assumed that if an earlier entry is not in
-- the database, then no later entries are (they are a monotonic subset of a
-- chain), but it's _not_ assumed that the next element is the child of the
-- previous.
bbsGetLcaMainChain
  :: DB IO
  -> (forall a . SlotNo -> Text -> IO a)
  -> OldestFirst [] HeaderHash
  -> IO (NewestFirst [] HeaderHash, OldestFirst [] HeaderHash)
bbsGetLcaMainChain db onErr (OldestFirst otherChain) = go [] otherChain
  where
  go !acc [] = pure (NewestFirst acc, OldestFirst [])
  go !acc (hh:older) = do
    inMain <- bbsGetBlockHeader db onErr hh
    case inMain of
      Nothing -> pure (NewestFirst acc, OldestFirst (hh:older))
      Just _  -> go (hh:acc) older

data BlockDecodeError where
  MalformedBlock :: !SlotNo -> !Text -> BlockDecodeError
  deriving (Show, Eq)

instance Exception BlockDecodeError

-- | An exception to throw in case 'Pos.Logic.Types.Logic.getTip' is called
-- when the database is empty.
data EmptyDatabaseError where
  EmptyDatabaseError :: EmptyDatabaseError
  deriving (Show, Eq)

instance Exception EmptyDatabaseError

-- | Bring up a Byron proxy.
--
-- The `DB` given must not be empty. If it is, `getTip` will throw an
-- exception. So be sure to seed the DB with the genesis block.
withByronProxy
  :: Tracer IO (Severity, Text.Builder)
  -> ByronProxyConfig
  -> DB IO
  -> (ByronProxy -> IO t)
  -> IO t
withByronProxy trace bpc db k =
  -- Create pools for all relayed data.
  -- TODO what about for delegation certificates?
  withPool (bpcPoolRoundInterval bpc) $ \(txPool :: TxPool) ->
  withPool (bpcPoolRoundInterval bpc) $ \(upProposalPool :: UpProposalPool) ->
  withPool (bpcPoolRoundInterval bpc) $ \(upVotePool :: UpVotePool) ->
  withPool (bpcPoolRoundInterval bpc) $ \(sscCommitmentPool :: SscCommitmentPool) ->
  withPool (bpcPoolRoundInterval bpc) $ \(sscOpeningPool :: SscOpeningPool) ->
  withPool (bpcPoolRoundInterval bpc) $ \(sscSharesPool :: SscSharesPool) ->
  withPool (bpcPoolRoundInterval bpc) $ \(sscVssCertPool :: SscVssCertPool) -> do

    -- The best announced block header, and the identifiers of every peer which
    -- announced it. `Nothing` whenever there is no known announcement. It will
    -- never go from `Just` to `Nothing`, it only starts as `Nothing`.
    tipsTVar :: TVar (Maybe (BestTip BlockHeader)) <- newTVarIO Nothing

    -- Send and receive bounded queues for atomic data (non-block).
    -- The receive queue is populated by the relay system by way of the logic
    -- layer. The send queue is emptied by a thread spawned here which uses
    -- the diffusion layer to send (ultimately by way of the outbound queue).
    atomRecvQueue :: TBQueue Atom <- newTBQueueIO (bpcRecvQueueSize bpc)
    atomSendQueue :: TBQueue Atom <- newTBQueueIO (bpcSendQueueSize bpc)

    let byronProxy :: Diffusion IO -> ByronProxy
        byronProxy diffusion = ByronProxy
          { bestTip = takeBestTip
          , downloadChain = streamBlocks diffusion
          , announceChain = announceBlockHeader diffusion
          , recvAtom = readTBQueue atomRecvQueue
          , sendAtom = writeTBQueue atomSendQueue
          }

        takeBestTip :: STM (Maybe (BestTip BlockHeader))
        takeBestTip = readTVar tipsTVar

        sendingThread :: forall x . Diffusion IO -> IO x
        sendingThread diffusion = do
          atom <- atomically $ readTBQueue atomSendQueue
          sendAtomToByron diffusion atom
          sendingThread diffusion

        blockDecodeError :: forall x . SlotNo -> Text -> IO x
        blockDecodeError slot text = throwIO $ MalformedBlock slot text

        mkLogic = \_diffusion -> Logic
          { -- This is only used to determine the message size limit on requesting
            -- block headers, which is sadly a byte limit on a message which contains
            -- _all_ of the headers. It's basically legacy, we'll be streaming blocks
            -- rather than batching.
            getAdoptedBVData   = pure (bpcAdoptedBVData bpc)
            -- Recovery mode is useless. We don't use it.
          , recoveryInProgress = pure False

            -- When a new block header announcement comes in, we update the best
            -- Byron tip `TVar`.
          , postBlockHeader    = \header peer -> atomically $
              modifyTVar' tipsTVar $ Just . updateBestTipMaybe peer header

            -- For these, we just need little mempools.
            -- Must be able to poke these mempools from Shelley, too
          , postTx            = taggedKeyValFromPool
              (Proxy :: Proxy TxMsgContents)
              (hash . taTx . getTxMsgContents)
              (atomically . writeTBQueue atomRecvQueue . Transaction)
              txPool
          , postUpdate        = taggedKeyValFromPool
              (Proxy :: Proxy (UpdateProposal, [UpdateVote]))
              (hash . fst)
              (atomically . writeTBQueue atomRecvQueue . UpdateProposal)
              upProposalPool
          , postVote          = taggedKeyValFromPool
              (Proxy :: Proxy UpdateVote)
              (\uv -> (uvProposalId uv, uvKey uv, uvDecision uv))
              (atomically . writeTBQueue atomRecvQueue . UpdateVote)
              upVotePool
          , postSscCommitment = taggedKeyValFromPool
              (Proxy :: Proxy MCCommitment)
              (\(MCCommitment (pk, _, _)) -> addressHash pk)
              (atomically . writeTBQueue atomRecvQueue . Commitment)
              sscCommitmentPool
          , postSscOpening    = taggedKeyValFromPool
              (Proxy :: Proxy MCOpening)
              (\(MCOpening key _) -> key)
              (atomically . writeTBQueue atomRecvQueue . Opening)
              sscOpeningPool
          , postSscShares     = taggedKeyValFromPool
              (Proxy :: Proxy MCShares)
              (\(MCShares key _) -> key)
              (atomically . writeTBQueue atomRecvQueue . Shares)
              sscSharesPool
          , postSscVssCert    = taggedKeyValFromPool
              (Proxy :: Proxy MCVssCertificate)
              (\(MCVssCertificate vc) -> getCertId vc)
              (atomically . writeTBQueue atomRecvQueue . VssCertificate)
              sscVssCertPool

            -- TODO FIXME what to do for this? Should we relay it using a pool?
            -- I'm not sure if the full diffusion/logic even does relaying of
            -- these.
          , postPskHeavy = \_ -> pure True

          -- Given a bunch of hashes, find LCA with main chain.
          -- With only the immutable, we just need to get the tip and that
          -- will be the LCA if it's in the set of hashes.
          --   tip
          , getLcaMainChain      = bbsGetLcaMainChain db blockDecodeError

          -- MsgGetHeaders conversation
          , Logic.getBlockHeader = bbsGetBlockHeader db blockDecodeError
          -- MsgGetHeaders conversation
          , getBlockHeaders      = \mLimit checkpoints mTip -> do
              result <- bbsGetBlockHeaders db blockDecodeError mLimit checkpoints mTip
              case result of
                Nothing -> pure $ Left $ GHFBadInput ""
                Just it -> pure $ Right it
          -- MsgGetHeaders conversation
          , getTip               = do
              dbTip <- DB.readTip db
              case dbTip of
                DB.TipGenesis -> do
                  traceWith trace (Error, "getTip: empty database")
                  throwIO $ EmptyDatabaseError
                DB.TipEBB slot hash bytes -> case decodeFull bytes of
                  Left cborError -> do
                    traceWith trace (Error, "getTip: malformed EBB")
                    throwIO $ MalformedBlock slot cborError
                  Right (Right _) -> do
                    traceWith trace (Error, "getTip: block where EBB expected")
                    throwIO $ MalformedBlock slot "block where EBB expected"
                  Right (Left ebb :: Block) -> pure $ Left ebb
                DB.TipBlock slot bytes -> case decodeFull bytes of
                  Left cborError -> do
                    traceWith trace (Error, "getTip: malformed block")
                    throwIO $ MalformedBlock slot cborError
                  Right (Left ebb) -> do
                    traceWith trace (Error, "getTip: EBB where block expected")
                    throwIO $ MalformedBlock slot "EBB where block expected"
                  Right (Right blk :: Block) -> pure $ Right blk
          -- GetBlocks conversation
          , getHashesRange       = \mLimit from to -> do
              result <- bbsGetHashesRange db blockDecodeError mLimit from to
              case result of
                Nothing -> pure $ Left $ GHRBadInput ""
                Just it -> pure $ Right it
          -- GetBlocks conversation
          , getSerializedBlock   = bbsGetSerializedBlock db
          -- StreamBlocks conversation
          --
          -- The conduit given to the continuation must _not_ yield the block
          -- at the given start hash! This is not documented in cardano-sl.
          , Logic.streamBlocks   = \hh k -> bracket (DB.readFrom db (DB.FromHash hh)) (DB.closeIterator) $ \iter -> do
              -- It's far easier to drop from the iterator than it is to drop
              -- from a conduit (conduit is weird).
              next <- DB.next (DB.iterator iter)
              let conduit = case next of
                    DB.Done -> pure ()
                    DB.NextBlock _ _ iter' -> DB.conduitFromIterator iter'
                    DB.NextEBB _ _ _ iter' -> DB.conduitFromIterator iter'
              k (mapOutput (Serialized . DB.dbBytes) conduit)
          }

        networkConfig = bpcNetworkConfig bpc
        fdconf = bpcDiffusionConfig bpc

    diffusionLayerFull fdconf networkConfig Nothing mkLogic $ \diffusionLayer -> do
      runDiffusionLayer diffusionLayer $ withAsync (sendingThread (diffusion diffusionLayer)) $
        \_ -> k (byronProxy (diffusion diffusionLayer))
