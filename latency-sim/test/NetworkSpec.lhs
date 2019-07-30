```{.haskell .hidden}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
module NetworkSpec where

import Network

import Data.Matrix
import Data.Ratio
import Data.Validity
import Data.Foldable(fold)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Test.Validity
import Test.Validity.Operations.Associativity
import Test.Validity.Operations.Commutativity
import Test.Validity.Operations.Identity
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
i  genValid = arbitrary `suchThat` isValid
```

Here we have properties typical of traditional instances of `Num`:
```{.haskell .literate}
specNegateIsSelfAdjoint x = negate (negate x) == x

specAddOnValid :: forall a. (Show a, Eq a, Num a, Arbitrary a, GenValid a) => String -> SpecWith ()
specAddOnValid description =
    (describe description $ do
      prop "commutativity"             $ commutativeOnValids @a (+)
      prop "associativity"             $ associativeOnValids @a (+)
      -- prop "fromInteger 0 is identity" $ identityOnValid     @a (+) $ fromInteger 0 -- need to know dimensions!
    ) :: (Show a, Eq a, Num a, Arbitrary a) => SpecWith ()

specMulOnValid :: forall a. (Show a, Eq a, Num a, Arbitrary a, GenValid a) => String -> SpecWith ()
specMulOnValid description =
    (describe description $ do
      prop "commutativity"             $ commutativeOnValids @a (*)
      prop "associativity"             $ associativeOnValids @a (*)
      -- prop "fromInteger 1 is identity" $ identityOnValid     @a (*) $ fromInteger 1) :: (Show a, Eq a, Num a, Arbitrary a) => SpecWith ()
      -- prop "fromInteger 0 is " $ identityOnValid     @a (*) $ fromInteger 1
    ) :: (Show a, Eq a, Num a, Arbitrary a) => SpecWith ()
```

Test that type class instances are valid:
```{.haskell .literate}
spec = do
  eqSpecOnValid        @(Matrix Int)
  shrinkValidSpec      @(Matrix Int)
  arbitrarySpec        @(Matrix Int)
  specAddOnValid       @(Matrix Int) "addition"
  specMulOnValid       @(Matrix Int) "multiplication"
```

To get specs to work we need a notion of matrix dimension.
Note that so far we are only interested in square matrices.

```{.haskell .literate}
data MMatrix (n::Natural) a = Matrix a
```

Additional tests planned:
* behaviour of a `Matrix Int`
* `Matrix Earliest` behaves like shortest distance
* `Matrix LatencyDistribution` converges
