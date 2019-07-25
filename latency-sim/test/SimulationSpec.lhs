```{.haskell .hidden}
{-# LANGUAGE TypeApplications#-}
module SimulationSpec(spec, expSim) where

import qualified Statistics.Distribution               as Statistics
import qualified Statistics.Distribution.Exponential   as Statistics
import qualified Statistics.Distribution.Uniform       as Statistics
import qualified Statistics.Distribution.Beta          as Statistics
import qualified Statistics.Distribution.CauchyLorentz as Statistics
import qualified Statistics.Distribution.Normal        as Statistics

import           Probability
import           Latency
import           Simulation
import           Test.Hspec hiding(after)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Property(ioProperty, (==>), Property)
import           Test.QuickCheck.Modifiers

import           LatencySpec hiding(spec)
```

Now we can test that our simulations of exponential distributions behave:
```{.haskell .literate}
expSim lambda = fromDistribution $ Statistics.exponential lambda
```
We need to check that `sampleSimulation` is a functor with respect to `TimeToCompletion` operations:
```{.haskell .literate}
shouldBeSimilarByDistribution :: Simulation -> Simulation -> Property
a `shouldBeSimilarByDistribution` b = ioProperty $ a `similarByDistribution` b

lawsOfTTCForSimulation :: Simulation -> Simulation -> Simulation -> Spec
lawsOfTTCForSimulation l m n = describe "TimeToCompletion laws on Simulation" $ do
  prop "noDelay is the same as delay 0" $ noDelay `shouldBeSimilarByDistribution` delay 0
  describe "basic laws of convolution" $ do
    prop "commutative"        $  (l `after` m) `shouldBeSimilarByDistribution` (m `after` l)
    prop "associative"        $ ((l `after` m) `after` n)
        `shouldBeSimilarByDistribution`  (l `after` (m `after` n))
    prop "delay 0 is neutral" $  (l `after` delay 0)
        `shouldBeSimilarByDistribution`   l
  describe "basic laws of firstToFinish" $ do
    prop "commutative"        $  (l `firstToFinish` m)
        `shouldBeSimilarByDistribution`  (m `firstToFinish` l)
    prop "associative"        $ ((l `firstToFinish` m) `firstToFinish` n)
        `shouldBeSimilarByDistribution`  (l `firstToFinish` (m `firstToFinish` n))
    prop "delay 0 is neutral" $  (l `firstToFinish` allLost)
        `shouldBeSimilarByDistribution`   l
  describe "basic laws of firstToFinish" $ do
    prop "commutative"        $  (l `lastToFinish` m)
        `shouldBeSimilarByDistribution`  (m `lastToFinish` l)
    prop "associative"        $ ((l `lastToFinish` m) `lastToFinish` n)
        `shouldBeSimilarByDistribution`  (l `lastToFinish` (m `lastToFinish` n))
    prop "delay 0 is neutral" $  (l `lastToFinish` delay 0)
        `shouldBeSimilarByDistribution`   l
```
Note that this verification is valid only for a particular distribution `d`.

We also add a one more distributions for extra confidence:
```{.haskell .literate}
uniformSim    :: Double -> Double -> Simulation
uniformSim a b = fromDistribution $ Statistics.uniformDistr a b
```

Summary of all tests is here:
```{.haskell .literate}
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
        (result `shouldBeSimilar`) <$> sampleSimulation (expSim lambda `lastToFinish` expSim mu)
  describe "laws on TTC on Simulation samples for chosen distributions with chosen parameters" $ do
    describe "exponential" $ lawsOfTTCForSimulation (expSim     1  ) (expSim     2  ) (expSim     3    )
    describe "uniform"     $ lawsOfTTCForSimulation (uniformSim 1 2) (uniformSim 3 4) (uniformSim 5   6)
    describe "uniform"     $ lawsOfTTCForSimulation (uniformSim 1 2) (expSim     2  ) (uniformSim 1   3)
```
