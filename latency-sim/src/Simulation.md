```{.haskell .hidden}
{-# LANGUAGE RankNTypes #-}
module Simulation where

import Control.Monad.Primitive(PrimMonad(..))
import Control.Monad(replicateM)
import GHC.Exts(IsList(..))
import qualified Statistics.Distribution as Statistics
import qualified System.Random.MWC as MWC(Gen, withSystemRandom)

import Probability
import Latency
```

## Verifying operations on distributions

We already use the type class `TimeToCompletion` to verify
bounds on distributions. Why not to use it that implementation
of `LatencyDistribution` is consistent with simple simulations?
We need additional basic operation: simulating a process with a given
distribution.

```{.haskell .literate}
class TimeToCompletion ttc => Stochastic ttc where
  stochasticProcess :: Statistics.ContGen d => d -> ttc
```

Now the simple simulation will just draw random delays for stochastic processes
and give us a total delay:
```{.haskell .literate}
newtype Simulation = Simulation {
    unSimulation :: (forall m. PrimMonad m => MWC.Gen (PrimState m) -> m Delay)
  }

instance TimeToCompletion Simulation where
  firstToFinish = onSimulation max
  lastToFinish  = onSimulation min
  after         = onSimulation (+)
  delay       t = Simulation $ const $ return t

onSimulation :: (Delay      -> Delay      -> Delay)
             ->  Simulation -> Simulation -> Simulation

onSimulation op a b = Simulation $ \st -> op <$> unSimulation a st
                                             <*> unSimulation b st

instance Stochastic Simulation where
  stochasticProcess d = Simulation sim
    where
      sim :: forall m. PrimMonad m => MWC.Gen (PrimState m) -> m Delay
      sim st = roundDelay <$> Statistics.genContVar d st
        where
          roundDelay :: Double -> Delay
          roundDelay = Delay . fromEnum . toInteger . truncate

```

Correctness of simulation lies on the assumption that we
can draw a random time for process once in simulation,
then operations are trivially implemented as minimum time, maximum time,
or addition of times for sequential composition.

Next we need to reconstruct distribution from multiple runs of simulation:

```{.haskell .literate}
sampleSimulation :: Int -> Simulation -> IO LatencyDistribution
sampleSimulation numSamples (Simulation s) = histogram <$>
    MWC.withSystemRandom sampler
  where
    sampler :: MWC.Gen (PrimState IO) -> IO [Delay]
    sampler st = replicateM numSamples (s st :: IO Delay)

histogram :: [Delay] -> LatencyDistribution
histogram = fromList . go (0, 0)
  where
    go (height, 0            ) []     = []
    go (_,      positiveCount) []     = [positiveCount]
    go (height, count        ) (d:ds) =
      if height == d
         then   go (height, count+1) ds
         else -- d>height
              0:go (height+1, 0) (d:ds)
```
