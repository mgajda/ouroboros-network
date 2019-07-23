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

import           LatencySpec hiding(spec)
```

Now we can test that our simulations of exponential distributions behave:
```{.haskell .literate}
expSim lambda = fromDistribution $ Statistics.exponential lambda

spec :: Spec
spec = parallel
     $ describe "simulations of exponential distributions behave well" $ do
  prop "firstToFinish of exponentials is an exponential with sum of parameters" $
      \(Positive lambda) (Positive mu) -> lambda>1.0 && mu>1.0 ==> ioProperty
                    ((expSim lambda `firstToFinish` expSim mu) `similarByDistribution`
                      expSim (lambda + mu))
  prop "lastToFinish of exponentials is an sum of exponentials with parameters lambda mu and lambda+mu" $
      \(Positive lambda) (Positive mu) -> lambda>1.0 && mu>1.0 ==> ioProperty $ do
        lambdaD   <- sampleSimulation $ expSim  lambda
        muD       <- sampleSimulation $ expSim         mu
        lambdaMuD <- sampleSimulation $ expSim (lambda+mu)
        let result = LatencyDistribution (pdf lambdaD + pdf muD - pdf lambdaMuD)
        (result `shouldBeSimilar`) <$> sampleSimulation (expSim lambda `firstToFinish` expSim mu)
```
