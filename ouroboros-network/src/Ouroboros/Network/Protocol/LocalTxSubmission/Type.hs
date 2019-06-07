{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- | The type of the transaction submission protocol.
--
-- This is used to relay transactions between nodes.
--
module Ouroboros.Network.Protocol.LocalTxSubmission.Type where


import           Network.TypedProtocol.Core


-- | The kind of the local transaction-submission protocol, and the types of
-- the states in the protocol state machine.
--
data LocalTxSubmission tx where

  -- | The client has agency; it can submit a transaction or terminate.
  --
  -- There is no timeout in this state.
  --
  StIdle   :: LocalTxSubmission tx

  -- | The server has agency; it must process the submitted transaction and
  -- either confirm or reject it.
  --
  -- There is a timeout in this state. If the mempool is full and remains so
  -- for a period then the transaction is rejected with the temporary failure
  -- reason.
  --
  StBusy   :: LocalTxSubmission tx

  -- | Nobody has agency; termination state.
  --
  StDone   :: LocalTxSubmission tx


instance Protocol (LocalTxSubmission tx) where

  -- | The messages in the transaction submission protocol.
  --
  -- In this protocol the client always initiates and the server replies.
  -- This makes it a push based protocol where the client manages the
  -- control flow. It is acceptable for this protocol to be push based
  -- because this protocol is only for use between a node and local client.
  --
  -- The protocol is a very simple request\/response pattern: a single
  -- transaction is submitted and it is either accepted or rejected.
  -- The confirmation or rejection (with reason) is returned.
  --
  data Message (LocalTxSubmission tx) from to where

    -- |
    --
    MsgSubmitTx
      :: tx
      -> Message (LocalTxSubmission tx) StIdle StBusy

    -- |
    --
    MsgAcceptTx
      :: Message (LocalTxSubmission tx) StBusy StIdle

    -- |
    --
    MsgRejectTx --TODO: add reject reason
      :: Message (LocalTxSubmission tx) StBusy StIdle

    -- | Termination message.
    --
    MsgDone
      :: Message (LocalTxSubmission tx) StIdle StDone


  data ClientHasAgency st where
    TokIdle  :: ClientHasAgency StIdle

  data ServerHasAgency st where
    TokBusy  :: ServerHasAgency StBusy

  data NobodyHasAgency st where
    TokDone  :: NobodyHasAgency StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


deriving instance Eq tx => Eq (Message (LocalTxSubmission tx) from to)

deriving instance Show tx => Show (Message (LocalTxSubmission tx) from to)

