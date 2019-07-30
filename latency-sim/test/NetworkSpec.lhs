```{.haskell .hidden}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
module NetworkSpec where

import Network

import Data.Matrix
import Data.Ratio
import Data.Validity
import Data.Foldable(fold)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Test.Validity
```

```{.haskell .literate}
instance Validity a => Validity (Matrix a) where
  validate = fold . fmap validate
  -- TODO: add a message which index is faulty?

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    Positive n <- arbitrary -- size constraint here? sqrt?
    Positive m <- arbitrary
    fromList n m <$>
      sequence [arbitrary | i<-[1..n], k<-[1..m]]
  shrink a = makeMinor <$> indices a
    where
      makeMinor (i, k) = minorMatrix i k a
      n = nrows a
      m = ncols a

elements a = map (a!) $ indices a
indices a = [(i,k) | i<-[1..n], k<-[1..m]]
  where
    n = nrows a
    m = ncols a

instance CoArbitrary a => CoArbitrary (Matrix a) where
  coarbitrary = foldr (.) id
              . fmap coarbitrary

instance Arbitrary a => GenUnchecked (Matrix a) where
  genUnchecked    = arbitrary
  shrinkUnchecked = shrink

instance (Validity         a
         ,Arbitrary        a)
      =>  GenValid (Matrix a) where
  genValid = arbitrary `suchThat` isValid
```

Test that type class instances are valid:
```{.haskell .literate}
spec = do
  eqSpecOnValid   @(Matrix Int)
  shrinkValidSpec @(Matrix Int)
  arbitrarySpec   @(Matrix Int)
```

Additional tests planned:
* behaviour of a `Matrix Int`
* `Matrix Earliest` behaves like shortest distance
* `Matrix LatencyDistribution` converges
