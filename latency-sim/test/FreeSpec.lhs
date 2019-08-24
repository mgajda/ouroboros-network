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
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module FreeSpec where

import Data.GenValidity
import Test.Hspec hiding (after)
import Test.Hspec.QuickCheck(prop)
import Test.QuickCheck.Gen
import Test.Validity.Eq
import Test.Validity.GenValidity
import Test.Validity.Operations.Commutativity
import Test.Validity.Operations.Associativity
import Test.Validity.Ord

import Free
import Latency

import DelaySpec
import ProbabilitySpec
```
## Free implementation of TimeToComplete

We use free, symbolic implementation to get nice validation
of certain properties of `TimeToComplete` algorithms.

Binary variants of operations for testing with validity:
```{.haskell .literate}
a `alt`  b = simplify $ a `firstToFinish` b
a `conj` b = simplify $ a `lastToFinish`  b
a `mul`  b = simplify $ a `after`         b
```

We also need instances to generate test inputs:
```{.haskell .literate}
instance GenUnchecked TVar where
  genUnchecked      = TVar <$> choose ('a', 'z')
  shrinkUnchecked _ = []

instance GenUnchecked FreeTTC

--instance Arbitrary FreeTTC where
--  arbitrary = genUnchecked
--  shrink    = shrinkUnchecked

instance Validity FreeTTC where
  validate a = (simplify a == a) `check` "simplified"

instance GenValid FreeTTC where
  genValid    = simplify <$> genUnchecked
  shrinkValid = fmap simplify . shrinkUnchecked
```

```{.haskell .literate}
spec = do
  describe "showing TTC" $ do
    describe "simple" $ do
      it "var" $
        show (Var (TVar 'd')) `shouldBe` "d"
      it "const" $
        show (Keep 1) `shouldBe` "1"
      it "zero" $
        show (Keep 0) `shouldBe` "0"
      it "delay" $
        show (Wait 1) `shouldBe` "+1t"
      it "d\\/d" $
        show (Alt  ["d","d"]) `shouldBe` "d\\/d"
      it "d/\\d" $
        show (Conj ["d","d"]) `shouldBe` "d/\\d"
      it "d;d" $
        show (Mul  ["d","d"]) `shouldBe` "d;d"
      it "d;d" $
        show (Alt  ["d", Mul  ["d","d"]]) `shouldBe` show (Alt  ["d", Mul  ["d","d"]])
    describe "combos" $ do
      it "altmul" $
        show (Alt [Mul ["d", "d"], Mul ["d", "d", "d"]])
          `shouldBe` "d;d\\/d;d;d"
  describe "laws" $ do
    describe "alt" $ do
      prop "commutative" $ commutativeOnValids alt
      prop "associative" $ associativeOnValids alt
    describe "conj" $ do
      prop "commutative" $ commutativeOnValids conj
      prop "associative" $ associativeOnValids conj
    describe "mul" $ do
      prop "commutative" $ commutativeOnValids mul
      prop "associative" $ associativeOnValids mul
  describe "classes" $ do
    eqSpecOnValid     @FreeTTC
    genValidSpec      @FreeTTC
    ordSpecOnValid    @FreeTTC
```

