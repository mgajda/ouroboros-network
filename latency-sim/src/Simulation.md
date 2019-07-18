```{.haskell .hidden}
{-# LANGUAGE RankNTypes #-}
module Simulation where

import           Control.Monad.Primitive(PrimMonad(..))
import           Control.Monad(replicateM)
import           Control.Monad.ST
import           Data.List(sort)
import           GHC.Exts(IsList(..))
import qualified Statistics.Distribution             as Statistics
import qualified System.Random.MWC as MWC(Gen, GenST, withSystemRandom, asGenST)

import           Probability
import           Latency
```

## Verifying operations on distributions

We already use the type class `TimeToCompletion` to verify
bounds on distributions. Why not to use it that implementation
of `LatencyDistribution` is consistent with simple simulations?
We need additional basic operation: simulating a process with a given
distribution.

```{.haskell .literate}
class TimeToCompletion ttc => Stochastic ttc where
  fromDistribution :: Statistics.ContGen d => d -> ttc
```

Now the simple simulation will just draw random delays for stochastic processes
and give us a total delay:
```{.haskell .literate}
newtype Simulation = Simulation {
    unSimulation :: forall s. MWC.GenST s -> ST s Delay
  }

instance TimeToCompletion Simulation where
  firstToFinish = onSimulation max
  lastToFinish  = onSimulation min
  after         = onSimulation (+)
  delay       t = Simulation $ const $ return t
  allLost       = Simulation $ const $ return maxBound

onSimulation :: (Delay      -> Delay      -> Delay)
             ->  Simulation -> Simulation -> Simulation

onSimulation op a b = Simulation $ \st -> op <$> unSimulation a st
                                             <*> unSimulation b st
```
Key part is generating a process of length with a given random distribution:
```{.haskell .literate}
instance Stochastic Simulation where
  fromDistribution d = Simulation sim
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

## Reconstructing the distribution from simulation

We need to sample `Simulation` a number of times to get a histogram.
```{.haskell .literate}
sampleSimulation' :: Int -> Simulation -> IO LatencyDistribution
sampleSimulation' numSamples (Simulation s) = histogram <$>
    (MWC.withSystemRandom . MWC.asGenST $ sampler)
  where
    sampler :: forall s. MWC.GenST s -> ST s [Delay]
    sampler st = replicateM numSamples $ s st -- :: ST s Delay)

sampleSimulation = sampleSimulation' 1000000
```
To build the histogram, we sort and count delays from the list of samples:
```{.haskell .literate}
histogram :: [Delay] -> LatencyDistribution
histogram = fromList . scale . go (0, 0) . sort
  where
    scale l = fmap (/sum l) l
    go (height, 0            ) []     = []
    go (_,      positiveCount) []     = [positiveCount]
    go (height, count        ) (d:ds) =
      if height == d
         then   go (height, count+1) ds
         else -- d>height
              count:go (height+1, 0) (d:ds)
```
To compare distributions we compute euclidean distance between their histograms:
```{.haskell .literate}
LatencyDistribution l `distance` LatencyDistribution m =
  unProb $ sum $ fmap (^2) $ l-m
```

Choosing `0.001` as similarity threshold (should depend on number of samples)
```{.haskell .literate}
a `similar` b = distance a b < 0.001
```
Now we can compare simulations too:
```{.haskell .literate}
s `similarByDistribution` t = similar <$> sampleSimulation s
                                      <*> sampleSimulation t
```