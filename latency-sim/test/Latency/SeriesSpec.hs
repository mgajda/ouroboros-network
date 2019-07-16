{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Latency.SeriesSpec(spec) where

import Series

import Test.QuickCheck
import Test.QuickCheck.All
import Test.Hspec
import Test.Hspec.QuickCheck

import GHC.Exts(IsList(..))


spec = do
  it "test simple convolution" $ convolve [1,1] [1,1] `shouldBe` ([1,2,1] :: Series Int)
  it "test series addition" $ [0,2,3,4,5] + [1] `shouldBe` ([1,2,3,4,5] :: Series Int)
  it "test probability of coincidence (elementwise multiplication)" $ [0.2, 0.4, 0.4] .*. [0.25,0.0,0.25,0.25] `shouldBe` ([0.05,0.0,0.1] :: Series Double)

