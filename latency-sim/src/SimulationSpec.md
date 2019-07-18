```{.haskell .hidden}
module SimulationSpec where

import qualified Statistics.Distribution             as Statistics
import qualified Statistics.Distribution.Exponential as Statistics

import           Probability
import           Latency
import           Simulation
import           Test.Hspec
import           Test.Hspec.QuickCheck
```

Now we can test that our simulations of exponential distributions behave:
```{.haskell .literate}
expSim lambda = fromDistribution $ Statistics.exponential lambda

spec = describe "simulations of exponential distributions behave well" $ do
  prop "firstToFinish of exponentials is an exponential with sum of parameters" $
    \lambda mu -> (expSim lambda ∨ expSim mu) `similarByDistribution`
                   expSim (lambda + mu)
```
