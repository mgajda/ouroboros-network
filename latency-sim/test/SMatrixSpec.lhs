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
### Testing matrix properties
```{.haskell .hidden}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}
module SMatrixSpec where

import Control.Monad(liftM2, liftM3)
import Data.Complex
import Data.Data
import Data.GenValidity
import Data.Proxy
import Data.Traversable
import Data.Typeable
import GHC.TypeLits

import qualified Data.Matrix as DM
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Test.Hspec hiding (after)
import Test.Validity.Eq
import Test.Validity.Shrinking
import Test.Validity.Arbitrary
import Test.Hspec.QuickCheck

import Latency
import NullUnit
import Probability
import SMatrix

import LatencySpec
```


```{.haskell .literate}
data GenSomeSMatrix a =
  forall n. KnownNat                     n
         => GenSomeSMatrix (Gen (SMatrix n a))

instance Validity            a
      => Validity (DM.Matrix a) where
  validate sm = foldMap validate sm

instance (Validity            a
         ,KnownNat          n  )
      =>  Validity (SMatrix n a) where
  validate (SMatrix sm) = validate sm

instance Validity              a
      => Validity (SomeSMatrix a) where
  validate (SomeSMatrix sm) = validate sm

instance Arbitrary                 a
      => GenUnchecked (SomeSMatrix a) where
  genUnchecked    = arbitrary
  shrinkUnchecked = shrink

instance (GenValid              a
         ,Arbitrary             a)
      =>  GenValid (SomeSMatrix a) where

instance Arbitrary SomeNat where
  arbitrary = do
    Positive a <- arbitrary
    case someNatVal a of
      Nothing -> error "Impossible when generating SomeNat"
      Just n  -> return n

instance Arbitrary              a
      => Arbitrary (SomeSMatrix a) where
  arbitrary = do
    randomSize <- arbitrary
    sMatrixOfSize randomSize

instance Arbitrary (GenSomeSMatrix a) where
  arbitrary = undefined

instance (Arbitrary            a
         ,KnownNat           n  )
       => Arbitrary (SMatrix n a) where
  arbitrary = sequenceA $ sMatrix Proxy (\_ -> arbitrary)

data Together a b =
  Together a b
  deriving (Eq, Ord, Functor, Foldable, Traversable, Show)

instance (Arbitrary           a
         ,Arbitrary             b)
      =>  Arbitrary (Together a b) where
  arbitrary = Together <$> arbitrary <*> arbitrary
  shrink (Together a b) = cartesianProduct (shrink a) (shrink b)

cartesianProduct = liftM2 Together

data Together3 a b c =
  Together3 a b c
  deriving (Eq, Ord, Show)

sMatrixOfSize :: Arbitrary a
              => SomeNat -> Gen (SomeSMatrix a)
sMatrixOfSize aSize = case aSize of
  SomeNat (Proxy :: Proxy n) ->
    SomeSMatrix <$> (arbitrary :: Arbitrary a => Gen (SMatrix n a))

test = u == u
  where
    u :: SMatrix 4 Int
    u = undefined

numMatrix :: Int -> SMatrix 1 Int
numMatrix a = sMatrix (Proxy :: Proxy 1) $ const a

sumMatrix :: (KnownNat n
             ,Num        a)
          =>  SMatrix  n a -> a
sumMatrix  = sum

(|*|) :: (Num        a
         ,KnownNat n  )
      =>  SMatrix  n a
      ->  SMatrix  n a
      ->  SMatrix  n a
(|*|) = sMatMult (+) (*)

complexMatrix :: Num a => Complex a -> SMatrix 2 a
complexMatrix c = sMatrixFromLists (Proxy :: Proxy 2)
                                   [[re, im], [-im, re]]
  where
    re = realPart c
    im = imagPart c

smatrixSpec =
  describe "SomeSMatrix" $ do
    eqSpecOnValid   @(SomeSMatrix Integer)
    shrinkValidSpec @(SomeSMatrix Integer)
    arbitrarySpec   @(SomeSMatrix Integer)

spec = do
  describe "Example operations on SMatrices" $ do
    it "SMatrix of size 1 is just a number" $ do
      numMatrix 1 == numMatrix 1
    prop "Addition of SMatrices of size 1 is just like number addition" $
      \a b -> sumMatrix (numMatrix a |+| numMatrix b) == a+b
    prop "Multiplication of SMatrices of size 1 is just like number multiplication" $
      \a b -> sumMatrix (numMatrix a |*| numMatrix b) == a*b
    describe "complex numbers as matrices" $ do
      prop "multiplication" $ \a (b :: Complex Double) ->
        complexMatrix a |*| complexMatrix b == complexMatrix (a*b)
  --eqSpecOnValid   @(SomeSMatrix Integer)
  --shrinkValidSpec @(SomeSMatrix Integer)
  --arbitrarySpec   @(SomeSMatrix Integer)

  {-describe "check properties of integers" $ do
    specAddOnGen  $ pure (arbitrary :: Gen Integer)
    specMulOnGen  $ pure (arbitrary :: Gen Integer)
  describe "check properties of matrices of integers" $ do
    specAddOnGen  $ genMatrix @Integer
    specMulOnGen  $ genMatrix @Integer -}
```
