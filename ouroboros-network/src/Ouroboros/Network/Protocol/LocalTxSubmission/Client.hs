{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}


-- | A view of the transaction submission protocol from the point of view of
-- the client.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, 'localTxSubmissionClientPeer' is provided for conversion
-- into the typed protocol.
--
module Ouroboros.Network.Protocol.LocalTxSubmission.Client (
    -- * Protocol type for the client
    -- | The protocol states from the point of view of the client.
    LocalTxSubmissionClient(..)

    -- * Execution as a typed protocol
  , localTxSubmissionClientPeer
  ) where

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type


-- | The client side of the local transaction submission protocol.
--
-- The peer in the client role submits transactions to the peer in the server
-- role.
--
data LocalTxSubmissionClient tx m a where

     SendMsgSubmitTx
       :: tx
       -> (Bool -> m (LocalTxSubmissionClient tx m a))
       -> LocalTxSubmissionClient tx m a

     SendMsgDone
       :: a -> LocalTxSubmissionClient tx m a


-- | A non-pipelined 'Peer' representing the 'LocalTxSubmissionClient'.
--
localTxSubmissionClientPeer :: forall tx m a. Monad m
                            => m (LocalTxSubmissionClient tx m a)
                            -> Peer (LocalTxSubmission tx) AsClient StIdle m a
localTxSubmissionClientPeer client =
    Effect $ go <$> client
  where
    go :: LocalTxSubmissionClient tx m a
       -> Peer (LocalTxSubmission tx) AsClient StIdle m a
    go (SendMsgSubmitTx tx k) =
      Yield (ClientAgency TokIdle)
            (MsgSubmitTx tx) $
      Await (ServerAgency TokBusy) $ \msg -> case msg of
        MsgAcceptTx -> Effect (go <$> k True)
        MsgRejectTx -> Effect (go <$> k False)

    go (SendMsgDone a) =
      Yield (ClientAgency TokIdle)
            MsgDone
            (Done TokDone a)

