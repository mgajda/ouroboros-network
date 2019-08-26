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

import Data.List(sort)

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
import NullUnit

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
  describe "simplification" $ do
    it "simplify Alt"    $ simplify (Alt  [Alt  ["a", "b"], "c"]) `shouldBe` Alt  ["a", "b", "c"]
    it "simplify Mul"    $ simplify (Mul  [Mul  ["a", "b"], "c"]) `shouldBe` Mul  ["a", "b", "c"]
    it "simplify Conj"   $ simplify (Conj [Conj ["a", "b"], "c"]) `shouldBe` Conj ["a", "b", "c"]
    it "simplify Alt  1 a" $ simplify (Alt  [unitE, "a"]) `shouldBe` unitE
    it "simplify Mul  1 a" $ simplify (Mul  [unitE, "a"]) `shouldBe` "a"
    it "simplify Conj 1 a" $ simplify (Conj [unitE, "a"]) `shouldBe` "a"
    it "simplify Alt  a 1" $ simplify (Alt  ["a", unitE]) `shouldBe` unitE
    it "simplify Mul  a 1" $ simplify (Mul  ["a", unitE]) `shouldBe` "a"
    it "simplify Conj a 1" $ simplify (Conj ["a", unitE]) `shouldBe` "a"
    it "simplify Alt  0 a" $ simplify (Alt  [nullE, "a"]) `shouldBe` "a"
    it "simplify Mul  0 a" $ simplify (Mul  [nullE, "a"]) `shouldBe` nullE
    it "simplify Conj 0 a" $ simplify (Conj [nullE, "a"]) `shouldBe` nullE
    it "simplify Alt  a 0" $ simplify (Alt  ["a", nullE]) `shouldBe` "a"
    it "simplify Mul  a 0" $ simplify (Mul  ["a", nullE]) `shouldBe` nullE
    it "simplify Conj a 0" $ simplify (Conj ["a", nullE]) `shouldBe` nullE
  describe "sorting" $ do
    it "a b"   $ sort ["a",     "b"    ] `shouldBe` ["a", "b"         :: FreeTTC]
    it "b a"   $ sort ["b",     "a"    ] `shouldBe` ["a", "b"         :: FreeTTC]
    it "a;a a" $ sort [mul "a" "a", "a"] `shouldBe` ["a", mul "a" "a" :: FreeTTC]
  describe "showing TTC" $ do
    describe "simple" $ do
      it "var" $
        showTTC (Var (TVar 'd')) `shouldBe` "d"
      it "const" $
        showTTC (Keep 1) `shouldBe` "1"
      it "zero" $
        showTTC (Keep 0) `shouldBe` "0"
      it "zero" $
        showTTC nullE `shouldBe` "0"
      it "delay" $
        showTTC (Wait 1) `shouldBe` "+1t"
      it "d∨d" $
        showTTC (Alt  ["d","d"]) `shouldBe` "d∨d"
      it "d∨0" $
        showTTC (Alt  ["d",nullE]) `shouldBe` "d"
      it "d∧d" $
        showTTC (Conj ["d","d"]) `shouldBe` "d∧d"
      it "d∧0" $
        showTTC (Conj  ["d",nullE]) `shouldBe` "0"
      it "d;d" $
        showTTC (Mul  ["d","d"]) `shouldBe` "d;d"
      it "d;0" $
        showTTC (simplify (Mul  ["d",nullE])) `shouldBe` "0"
      it "d;0 ++" $
        simplify (Mul  ["d",nullE]) `shouldBe` nullE
      it "d;d" $
        showTTC (Alt  [Mul  ["d","d"], "d"]) `shouldBe` showTTC (Alt  ["d", Mul  ["d","d"]])
    describe "combos" $ do
      it "altmul" $
        showTTC (Alt [Mul ["d", "d"], Mul ["d", "d", "d"]])
          `shouldBe` "d;d∨d;d;d"
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

