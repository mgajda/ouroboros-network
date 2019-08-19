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
module FreeSpec where

import Test.Hspec

import Free
```


```{.haskell .literate}
spec = do
  describe "showing TTC" $ do
    describe "simple" $ do
      it "var" $
        show (Var 'd') `shouldBe` "d"
      it "const" $
        show (Keep 1.0) `shouldBe` "1.0"
      it "delay" $
        show (Wait 1) `shouldBe` "+1t"
      it "d\\/d" $
        show (Alt ["d","d"]) `shouldBe` "d\\/d"
      it "d/\\d" $
        show (Conj ["d","d"]) `shouldBe` "d/\\d"
      it "d;d" $
        show (Mul  ["d","d"]) `shouldBe` "d;d"
    describe "combos" $ do
      it "altmul" $
        show (Alt [Mul ["d", "d"], Mul ["d", "d", "d"]])
          `shouldBe` "d;d\\/d;d;d"
```
