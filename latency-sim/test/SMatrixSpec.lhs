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
{-# LANGUAGE DeriveFunctor              #-}
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
import Data.Proxy
import Data.Traversable
import GHC.TypeLits

import NullUnit
import SMatrix

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Test.Hspec
```


```{.haskell .literate}
data GenSomeSMatrix a =
  forall n. KnownNat                     n
         => GenSomeSMatrix (Gen (SMatrix n a))

data SomeSMatrix a =
   forall n. KnownNat             n
          => SomeSMatrix (SMatrix n a)

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

spec = do
  it "truthy" $ True `shouldBe` True
  --eqSpecOnValid   @(SMatrix 3 Int)
  --shrinkValidSpec @(SMatrix 3 Int)
  --arbitrarySpec   @(SMatrix 3 Int)
  {-describe "check properties of integers" $ do
    specAddOnGen  $ pure (arbitrary :: Gen Integer)
    specMulOnGen  $ pure (arbitrary :: Gen Integer)
  describe "check properties of matrices of integers" $ do
    specAddOnGen  $ genMatrix @Integer
    specMulOnGen  $ genMatrix @Integer -}
```
