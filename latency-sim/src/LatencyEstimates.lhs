```{.haskell .hidden}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}
module LatencyEstimates where

import GHC.Exts(IsList(..))
import Control.Monad.Primitive(PrimMonad(..))
import Control.Monad(replicateM)
import Data.Function(on)
import Data.Semigroup
import qualified Statistics.Distribution as Statistics(ContGen(..))
import qualified System.Random.MWC as MWC(Gen, withSystemRandom)
import Test.QuickCheck
import Test.Hspec(describe, SpecWith)
import Test.Hspec.QuickCheck(prop)

import Probability
import Delay
import Series
import Latency as L
```

## Bounds on distributions

Note that we can define bounds on `LatencyDistribution` that behave like functors
over basic operations from `TimeToCompletion` class.

* Upper bound on distribution is the `Latest` possible time^[Here `liftBinOp` is for lifting an operator to a newtype.]:
```{.haskell .literate}
newtype Latest = Latest { unLatest :: Delay }
  deriving (Eq, Ord, Show)

latest :: LatencyDistribution -> Latest
latest [0.0] = Latest maxBound
latest  x    = Latest . Delay . (-1+) . length . unSeries . prob $ x

onLatest = liftBinOp unLatest Latest

instance TimeToCompletion Latest where
  firstToFinish = onLatest min
  lastToFinish  = onLatest max
  after         = onLatest (+)
  delay         = Latest
  allLost       = Latest maxBound -- TODO: is it clear, or use Maybe?
```
* Lower bound on distribution is the `Earliest` possible time:
```{.haskell .literate}
newtype Earliest = Earliest { unEarliest :: Delay }
  deriving (Eq, Ord, Show)
earliest :: LatencyDistribution -> Earliest
earliest [0.0]                           = Earliest maxBound
earliest [_]                             = Earliest 0
earliest (last . unSeries . prob -> 0.0) = error "Canonical LatencyDistribution should always end with non-zero value"
earliest  other                          = Earliest . Delay . (max 0) . length . takeWhile (0==) . unSeries . prob $ other

onEarliest = liftBinOp unEarliest Earliest

instance TimeToCompletion Earliest where
  firstToFinish = onEarliest min
  lastToFinish  = onEarliest max
  after         = onEarliest (+)
  delay         = Earliest
  allLost       = Earliest maxBound -- TODO: is it clear?
```

These estimates have the property that we can easily compute
the same operations on estimates, without really computing
the full `LatencyDistribution`.

```{.haskell .hidden}
-- | Lift binary operator to newtype.
--   That should probably be in standard library, but is usually derived on newtypes.
liftBinOp unpack pack op a b = pack (unpack a `op` unpack b)
```
