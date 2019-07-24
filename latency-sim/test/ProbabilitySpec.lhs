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
instance Validity Probability where
  validate p = check (p>=0) "probability is greater than 0.0"
            <> check (p<=1) "probability is less than 1.0"

instance GenUnchecked Probability where
  genUnchecked    = arbitrary
  shrinkUnchecked = shrink

instance GenValid Probability where
  genValid = arbitrary `suchThat` isValid

deriving instance CoArbitrary Probability

instance Arbitrary Probability where
  arbitrary = sized $ \aSize -> do
      let precision = fromIntegral (aSize `max` startingPrecision)
      denominator <- choose (1, precision  )
      numerator   <- choose (0, denominator)
      pure         $ Prob (fromRational (numerator % denominator))
    where
      startingPrecision = 5
  shrink (Prob x) = filter isValidProbability (Prob <$> shrink x)
```

Test that type class instances are valid:
```{.haskell .literate}
spec = do
  eqSpecOnValid   @Probability
  ordSpecOnValid  @Probability
  showReadSpec    @Probability
  shrinkValidSpec @Probability
  arbitrarySpec   @Probability
```
