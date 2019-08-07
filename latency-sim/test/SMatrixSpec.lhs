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
module SMatrixSpec where

import Control.Monad(liftM2, liftM3)
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
import Test.Hspec

import NullUnit
import SMatrix
```


```{.haskell .literate}
data GenSomeSMatrix a =
  forall n. KnownNat                     n
         => GenSomeSMatrix (Gen (SMatrix n a))

data SomeSMatrix a =
   forall n. KnownNat             n
          => SomeSMatrix (SMatrix n a)
    deriving (Typeable)

instance Eq a => Eq (SomeSMatrix a) where
  SomeSMatrix a == SomeSMatrix b = False

instance Show a => Show (SomeSMatrix a) where
  showsPrec _ (SomeSMatrix sm) = ("SMatrix " ++) . shows sm

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

smatrixGen = do
  Positive n <- arbitrary
  return $ someNatVal n

sMatrixOfSize :: Arbitrary a
              => SomeNat -> Gen (SomeSMatrix a)
sMatrixOfSize aSize = case aSize of
  SomeNat (Proxy :: Proxy n) ->
    SomeSMatrix <$> (arbitrary :: Gen (SMatrix n _))

test = u == u
  where
    u :: SMatrix 4 Int
    u = undefined

numMatrix :: Int -> SMatrix 1 Int
numMatrix a = sMatrix (Proxy :: Proxy 1) $ const a

spec = do
  describe "Example operations on SMatrices" $ do
    it "SMatrix of size 1 is just a number" $ do
      numMatrix 1 == numMatrix 1
  it "truthy" $ True `shouldBe` True
  --eqSpecOnValid @(SMatrix 4 Int)
  --shrinkValidSpec @(SomeSMatrix Integer)
  --arbitrarySpec   @(SomeSMatrix Integer)
  {-describe "check properties of integers" $ do
    specAddOnGen  $ pure (arbitrary :: Gen Integer)
    specMulOnGen  $ pure (arbitrary :: Gen Integer)
  describe "check properties of matrices of integers" $ do
    specAddOnGen  $ genMatrix @Integer
    specMulOnGen  $ genMatrix @Integer -}
```
