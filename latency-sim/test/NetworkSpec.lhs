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
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
module NetworkSpec where

import Latency
import Network
import Series
import NullUnit

import Control.Monad
import Control.Exception(assert)
import Data.Matrix
import Data.Ratio
import Data.Validity
import Data.Foldable(fold)
import GHC.TypeNats

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Test.Validity
import Test.Validity.Operations.Associativity
import Test.Validity.Operations.Commutativity
import Test.Validity.Operations.Identity
```

# Appendix: validation of operations on the networks and matrices

Note that in the context of this work, we are only interested
in connection matrices. They have the following properties:
* they are always square matrices
* they always have unit distribution (`noDelay`) on the diagonal

```{.haskell .literate}
instance (Unit            a
         ,Eq              a
         ,Validity        a)
      => Validity (Matrix a) where
  validate a = mconcat
      [connMatrixElement i k | i<-[1..n], k<-[1..m]]
    where
      n = nrows a
      m = ncols a
      connMatrixElement i k | i == k =
        (a ! (i,k) == unitE) `check` "diagonal element is unit"
      connMatrixElement i k | i /= k =
        validate (a ! (i,k))

instance (Unit              a
         ,Arbitrary         a)
      =>  Arbitrary (Matrix a) where
  arbitrary = do
      n <- choose (1,20) -- No change expected for larger matrices
      genConnMatrix n

  shrink a | nrows a == 1 = []
  shrink a = makeMinor <$> indices a
    where
      makeMinor (i, k) = minorMatrix i k a
      n = nrows a
      m = ncols a
```
To generate connection matrix of size $n$:
```{.haskell .literate}
genConnMatrix :: (Arbitrary  a
                 ,Unit       a)
              => Int
              -> Gen (Matrix a)
genConnMatrix n = do
  fromList n n <$> -- we are interested in square matrices only
    sequence [genConnElt i k | i<-[1..n]
                             , k<-[1..n]]
```
When we are interested in generating multiple matrices of the same size,
we can generate a generate for a random size:
```{.haskell .literate}
genGenConnMatrix :: (Unit             a
                    ,Null             a
                    ,Arbitrary        a)
                 =>  Gen (GenIN (Matrix a))
genGenConnMatrix  = do
  n <- choose (1,20)
  return $ GenIN { genTest = genConnMatrix n
                 , genUnit = unitSized     n
                 , genNull = nullSized     n
                 }

swapGenConnMatrix genGen = do
  GenIN {..} <- genGen
  return $ GenIN { genTest = genTest
                 , genUnit = genNull
                 , genNull = genUnit
                 }

unitSized n = matrix n n unitElt

nullSized n = matrix n n $ const nullE

unitElt (i,j) | i==j = unitE
unitElt _            = nullE
```

Generation of element in connection matrix depends on index:
```{.haskell .literate}
genConnElt i j | i == j = return unitE
genConnElt _ _          = arbitrary

elements a = map (a!) $ indices a

indices a = [(i,k) | i<-[1..n]
                   , k<-[1..m]]
  where
    n = nrows a
    m = ncols a

instance CoArbitrary         a
      => CoArbitrary (Matrix a) where
  coarbitrary = foldr (.) id
              . fmap coarbitrary

instance (Arbitrary           a
         ,Unit                a)
      => GenUnchecked (Matrix a) where
  genUnchecked    = arbitrary
  shrinkUnchecked = shrink

instance (Unit             a
         ,Eq               a
         ,Validity         a
         ,Arbitrary        a)
      =>  GenValid (Matrix a) where
  genValid = arbitrary `suchThat` isValid
```

Here we have properties typical of traditional instances of `Num`:
```{.haskell .literate}
identityWithGen :: Gen (Gen a, a) -> Property
identityWithGen = undefined

specNegateIsSelfAdjoint x = negate (negate x) == x

-- | Generator of both generator, null, and identity.
data GenIN a =
  GenIN {
    genTest :: Gen a
  , genUnit :: a
  , genNull :: a
  }

specACIOnGenGen :: forall a. (Show      a
                             ,Eq        a
                             ,Arbitrary a
                             ,GenValid  a)
                => (a -> a -> a)
                -> Gen (GenIN a)
                -> String
                -> SpecWith ()
specACIOnGenGen op gen description =
    describe description $ do
      --prop "commutativity"             $
      --  commutativeOnGens op   pairGen    pairShrink
      prop "associativity"                   $
        associativeOnGens op   tripleGen  tripleShrink
      prop "neutral element" $ -- this needs better types
        identityWithGenIN op gen
      --prop "null element" $ -- this needs better types
      --  nullWithGenIN     op gen shrink
  where
    pairShrink :: (a,a) -> [(a,a)]
    pairShrink   (a,b)   = zip  (shrink a) (shrink b)
    tripleShrink (a,b,c) = zip3 (shrink a) (shrink b) (shrink c)
    pairGen              = do
      GenIN {genTest} <- gen
      (,)  <$> genTest <*> genTest
    tripleGen            = do
      GenIN {genTest} <- gen
      (,,) <$> genTest <*> genTest <*> genTest
    specialGen = undefined

nullWithGenIN     op genIn shrink = do
  GenIN {genTest, genNull} <- genIn
  aTest <- genTest
  assert   ((aTest   `op` genNull) == genNull) $
    return ((genNull `op` aTest  ) == genNull)

identityWithGenIN op genIn = do
  GenIN {genTest, genUnit} <- genIn
  aTest <- genTest
  assert   ((aTest   `op` genUnit) == aTest) $
    return ((genUnit `op` aTest  ) == aTest)

specAddOnGen :: forall a. (Show      a
                          ,Eq        a
                          ,Num       a
                          ,Arbitrary a
                          ,GenValid  a)
             => Gen (GenIN a) -> SpecWith ()
specAddOnGen genIN = specACIOnGenGen (+) genIN "addition"

```
Or should I use `forAllShrink :: (Show a, Testable prop) => Gen a -> (a -> [a]) -> (a -> prop) -> Property`?
```{.haskell .literate}
specMulOnGen :: forall a. (Show      a
                          ,Eq        a
                          ,Num       a
                          ,Arbitrary a
                          ,GenValid  a)
             => Gen (GenIN a)
             -> SpecWith ()
specMulOnGen gen = specACIOnGenGen (*) gen "multiplication"

a `sameDim` b = (nrows a == nrows b)
```

Test that type class instances are valid:
```{.haskell .literate}
spec = do
  eqSpecOnValid   @(Matrix Int)
  shrinkValidSpec @(Matrix Int)
  arbitrarySpec   @(Matrix Int)
  describe "check properties of integers" $ do
    specAddOnGen  $ pure $ GenIN (arbitrary :: Gen Integer) 0 0
    specMulOnGen  $ pure $ GenIN (arbitrary :: Gen Integer) 1 0
  describe "check properties of matrices of integers" $ do
    specAddOnGen  $ swapGenConnMatrix $ genGenConnMatrix @Integer
    specMulOnGen  $ genGenConnMatrix @Integer
```

To get specs to work we need a notion of matrix dimension.
Note that so far we are only interested in square matrices.

Additional tests planned:

* behaviour of a `Matrix Int`
* `Matrix Earliest` behaves like shortest distance
* `Matrix LatencyDistribution` converges
