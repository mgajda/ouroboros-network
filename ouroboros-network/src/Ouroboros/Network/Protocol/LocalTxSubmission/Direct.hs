{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.LocalTxSubmission.Direct (
    direct
  ) where

import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
import           Ouroboros.Network.Protocol.LocalTxSubmission.Server


direct :: forall tx m a b.
          Monad m
       => LocalTxSubmissionClient tx m a
       -> LocalTxSubmissionServer tx m b
       -> m (a, b)
direct (SendMsgSubmitTx tx k) LocalTxSubmissionServer{recvMsgSubmitTx} = do
    (res, server') <- recvMsgSubmitTx tx
    client' <- k res
    direct client' server'

direct (SendMsgDone a) LocalTxSubmissionServer{recvMsgDone = b} =
    return (a,b)

