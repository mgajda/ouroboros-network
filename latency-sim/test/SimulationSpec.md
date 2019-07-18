```{.haskell .hidden}
module SimulationSpec(spec, expSim) where

import qualified Statistics.Distribution             as Statistics
import qualified Statistics.Distribution.Exponential as Statistics

import           Probability
import           Latency
import           Simulation
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Property(ioProperty, (==>))
import           Test.QuickCheck.Modifiers
```

Now we can test that our simulations of exponential distributions behave:
```{.haskell .literate}
expSim lambda = fromDistribution $ Statistics.exponential lambda

spec :: Spec
spec = describe "simulations of exponential distributions behave well" $ do
  prop "firstToFinish of exponentials is an exponential with sum of parameters" $
      \(Positive lambda) (Positive mu) -> lambda>1.0 && mu>1.0 ==> ioProperty
                    ((expSim lambda ∨ expSim mu) `similarByDistribution`
                      expSim (lambda + mu))
```
