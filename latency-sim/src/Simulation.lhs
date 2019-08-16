---
input: markdown+tex_math_dollars+yaml_metadata_block+citations
output:
  pdf_document:
    keep_tex: true
    toc: true
    toc_depth: 2
    latex_engine: xelatex
bibliography:
  - Latency.bib
---
```{.haskell .hidden}
{-# LANGUAGE RankNTypes #-}
module Simulation where

import           Control.Monad(replicateM)
import           Control.Monad.ST
import           Data.List(sort)
import           GHC.Exts(IsList(..))
import qualified Statistics.Distribution             as Statistics
import qualified System.Random.MWC as MWC(Gen, GenST, withSystemRandom, asGenST)

import           Latency
import           Probability
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
  firstToFinish = onSimulation min
  lastToFinish  = onSimulation max
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
      sim :: MWC.GenST s -> ST s Delay
      sim st = roundDelay <$> Statistics.genContVar d st
        where
          roundDelay :: Double -> Delay
          roundDelay = Delay . fromEnum . toInteger . ceiling
```

Correctness of simulation lies on the assumption that we
can draw a random time for process once in simulation,
then operations are trivially implemented as minimum time, maximum time,
or addition of times for sequential composition.

## Reconstructing the distribution from simulation

We need to sample `Simulation` a number of times to get a histogram.
```{.haskell .literate}
sampleSimulation' :: Probability a
                  => Int -> Simulation -> IO (LatencyDistribution a)
sampleSimulation' numSamples (Simulation s) = histogram <$>
    (MWC.withSystemRandom . MWC.asGenST $ sampler)
  where
    sampler :: forall s. MWC.GenST s -> ST s [Delay]
    sampler st = replicateM numSamples $ s st

sampleSimulation :: Simulation
                 -> IO (LatencyDistribution ApproximateProbability)
sampleSimulation  = sampleSimulation' 10000
```
To build the histogram, we sort and count delays from the list of samples:
```{.haskell .literate}
histogram :: Probability         a
          => [Delay]
          -> LatencyDistribution a
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
Now we can compare simulations too:
```{.haskell .literate}
s `similarByDistribution` t = (~~) <$> sampleSimulation s
                                   <*> sampleSimulation t
```
