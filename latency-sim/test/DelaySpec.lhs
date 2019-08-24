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
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
module DelaySpec where

import Data.GenValidity
import Test.Validity.Arbitrary
import Test.Validity.GenValidity
import Test.Validity.Eq
import Test.Validity.Ord
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen(suchThat)

import Delay
```

```{.haskell .literate}
instance GenUnchecked Delay where
  genUnchecked = Delay <$> genUnchecked

instance Validity Delay where
  validate (Delay d) = (d>=0) `check` "delay should be non-negative"

instance GenValid Delay where
  genValid    = genUnchecked `suchThat` isValid
  shrinkValid = filter isValid . shrinkUnchecked

instance Arbitrary Delay where
  arbitrary = getNonNegative <$> arbitrary
  shrink (Delay d) = Delay <$> shrinkIntegral d

deriving instance CoArbitrary Delay

spec = do
  eqSpecOnValid     @Delay
  eqSpecOnArbitrary @Delay
  genValidSpec      @Delay
  ordSpecOnValid    @Delay
```

