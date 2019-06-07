{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Ouroboros.Network.Protocol.LocalTxSubmission.Examples (
    localTxSubmissionClient,
    localTxSubmissionServer,
  ) where

import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
import           Ouroboros.Network.Protocol.LocalTxSubmission.Server



--
-- Example client
--

-- | An example @'LocalTxSubmissionClient'@ which submits a fixed list of
-- transactions. The result is those transactions annotated with whether they
-- were accepted or rejected.
--
localTxSubmissionClient :: forall tx m.
                           Applicative m
                        => [tx]
                        -> LocalTxSubmissionClient tx m [(tx, Bool)]
localTxSubmissionClient =
    client []
  where
    client acc [] =
      SendMsgDone (reverse acc)

    client acc (tx:txs) =
      SendMsgSubmitTx tx $ \ok -> pure (client ((tx,ok):acc) txs)


--
-- Example server
--

localTxSubmissionServer :: forall tx m.
                           (Show tx, Applicative m)
                        => (tx -> Bool)
                        -> LocalTxSubmissionServer tx m [(tx, Bool)]
localTxSubmissionServer p =
    server []
  where
    server acc = LocalTxSubmissionServer {
      recvMsgSubmitTx = \tx ->
        let ok = p tx in
        pure (ok, server ((tx, ok) : acc)),

      recvMsgDone = reverse acc
    }

