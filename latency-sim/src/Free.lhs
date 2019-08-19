
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
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}
module Free(
    FreeTTC(..)
  , simplify
  ) where

import GHC.Exts(IsList(..))
import Control.Monad(replicateM)
import Data.Function(on)
import Data.String
import Data.List(partition,sort)
import Data.Maybe
import Data.Semigroup
import Data.Typeable
import GHC.Generics
import Data.Validity
import Test.QuickCheck
import Data.Generics.Uniplate.Data
import Data.Data

import Probability
import Delay
import Latency
import Series
import Metric
import NullUnit
import Complement
```
Here we interpret `TimeToCompletion` as free algebra to get algebraic descriptions
of our test cases.

```{.haskell .literate}
data FreeTTC =
    Var   Char
  | Keep  Double
  | Wait  Delay
  | Alt  [FreeTTC]
  | Conj [FreeTTC]
  | Mul  [FreeTTC]
  deriving (Eq, Ord, Data, Typeable, Generic)

fromAlt (Alt alts) = alts
fromAlt (Keep 0.0) = []
fromAlt  other     = [other]

fromMul (Mul coefs) = coefs
fromMul (Keep 1.0 ) = []
fromMul  other      = [other]

fromConj (Conj  coefs) = coefs
fromConj (Keep  1.0  ) = []
fromConj (Wait  0    ) = []
fromConj  other        = [other]

simpXform (Alt  as ) = Alt  $ sort $ concatMap fromAlt  as
simpXform (Mul  ms ) = Mul  $ sort $ concatMap fromMul  ms
simpXform (Conj cs ) = Conj $ sort $ concatMap fromConj cs
simpXform (Keep 1.0) = Wait 0
simpXform  other     = other

simplify = transform simpXform
-- TODO: add reduction of constant-exprs? (Keep, Delay)

instance IsString FreeTTC where
  fromString [c]   = Var c
  fromString other = error $ "Only single character strings are allowed as FreeTTC variables. Got: "
                          ++ show other

dyadic op a b = op [a,b]

instance TimeToCompletion FreeTTC where
  firstToFinish = dyadic Alt
  lastToFinish  = dyadic Conj
  after         = dyadic Mul
  delay         = Wait
  allLost       = Keep 0
  noDelay       = Keep 1


```


