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
{-# LANGUAGE ScopedTypeVariables #-}
module ShowUtilsSpec where

import Test.Hspec

import ShowUtils
```


```{.haskell .literate}
spec = do
  describe "showing surfaces" $ do
    it "joinLines" $
      joinLines [("Alpha"++), ("Beta"++), ("Gamma"++)] ""
        `shouldBe` "Alpha\nBeta\nGamma"
    it "joins" $
      joins ("\n"++) ("Alpha"++) ("Beta"++) "" `shouldBe` "Alpha\nBeta"
```
