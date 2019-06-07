{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A view of the transaction submission protocol from the point of view of
-- the server.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Ouroboros.Network.Protocol.LocalTxSubmission.Server (
    -- * Protocol type for the server
    -- | The protocol states from the point of view of the server.
    LocalTxSubmissionServer (..)

    -- * Execution as a typed protocol
  , localTxSubmissionServerPeer
  ) where

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type


-- | The server side of the local transaction submission protocol.
--
-- The peer in the client role submits transactions to the peer in the server
-- role.
--
data LocalTxSubmissionServer tx m a =
     LocalTxSubmissionServer {

       -- |
       --
       recvMsgSubmitTx :: tx -> m (Bool, LocalTxSubmissionServer tx m a),

       -- |
       --
       recvMsgDone     :: a
     }


-- | A non-pipelined 'Peer' representing the 'LocalTxSubmissionServer'.
--
localTxSubmissionServerPeer :: forall tx m a. Monad m
                            => m (LocalTxSubmissionServer tx m a)
                            -> Peer (LocalTxSubmission tx) AsServer StIdle m a
localTxSubmissionServerPeer server =
    Effect $ go <$> server
  where
    go :: LocalTxSubmissionServer tx m a
       -> Peer (LocalTxSubmission tx) AsServer StIdle m a
    go LocalTxSubmissionServer{recvMsgSubmitTx, recvMsgDone} =
      Await (ClientAgency TokIdle) $ \msg -> case msg of
        MsgSubmitTx tx -> Effect $ do
          (ok, k) <- recvMsgSubmitTx tx
          return $
            case ok of
              True  -> Yield
                         (ServerAgency TokBusy)
                         MsgAcceptTx
                         (go k)
              False -> Yield
                         (ServerAgency TokBusy)
                         MsgRejectTx
                         (go k)

        MsgDone -> Done TokDone recvMsgDone

