```{.haskell .hidden}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
module ProbabilitySpec where

import Probability

import Data.Ratio
import Data.Validity

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.Validity
```

```{.haskell .literate}
instance Validity IdealizedProbability where
  validate p = check (p>=0) "probability is greater than 0.0"
            <> check (p<=1) "probability is less than 1.0"

instance Validity ApproximateProbability where
  validate p = check (p>=0)         "probability is greater than 0.0"
            <> check (p<=1+epsilon) "probability is less than 1.0"

epsilon = 1e-12

instance GenUnchecked IdealizedProbability where
  genUnchecked    = arbitrary
  shrinkUnchecked = shrink

instance GenUnchecked ApproximateProbability where
  genUnchecked    = arbitrary
  shrinkUnchecked = shrink

instance GenValid IdealizedProbability where
  genValid = arbitrary `suchThat` isValid

instance GenValid ApproximateProbability where
  genValid = arbitrary `suchThat` isValid

deriving instance CoArbitrary IdealizedProbability

deriving instance CoArbitrary ApproximateProbability

instance Arbitrary ApproximateProbability where
  arbitrary = sized $ \aSize -> do
      let precision = fromIntegral (aSize `max` startingPrecision)
      denominator <- choose (1, precision  )
      numerator   <- choose (0, denominator)
      pure         $ AProb (fromRational (numerator % denominator))
    where
      startingPrecision = 5
  shrink (AProb x) = filter isValidApproximateProbability (AProb <$> shrink x)

instance Arbitrary IdealizedProbability where
  arbitrary = sized $ \aSize -> do
      let precision = fromIntegral (aSize `max` startingPrecision)
      denominator <- choose (1, precision  )
      numerator   <- choose (0, denominator)
      pure         $ IProb (fromRational (numerator % denominator))
    where
      startingPrecision = 5
  shrink (IProb x) = filter isValidIdealizedProbability
                            (IProb <$> shrink x)
```

Test that type class instances are valid:
```{.haskell .literate}
spec = do
  eqSpecOnValid   @IdealizedProbability
  ordSpecOnValid  @IdealizedProbability
  shrinkValidSpec @IdealizedProbability
  arbitrarySpec   @IdealizedProbability
  eqSpecOnValid   @ApproximateProbability
  ordSpecOnValid  @ApproximateProbability
  shrinkValidSpec @ApproximateProbability
  arbitrarySpec   @ApproximateProbability
```
