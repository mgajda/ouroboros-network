{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE OverloadedLists #-}
module Latency.SeriesSpec where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import Test.Hspec
import Test.Hspec.QuickCheck

import GHC.Exts(IsList(..))

import Delay
import Series

instance Arbitrary a => Arbitrary (Series a) where
  arbitrary = Series <$> arbitrary

deriving instance CoArbitrary a => CoArbitrary (Series a)

pascalTriangle :: Integer -> Series Integer
pascalTriangle row = Series [nk row column | column <- [0..row]]

nk :: Integer -> Integer -> Integer
nk n _ | n < 0 = error "For nk, n must be nonnegative"
nk _ k | k < 0 = error "For nk, k must be nonnegative"
nk n k | n < k = error "For nk, k must be no greater than n!"
nk n k = (factorial n `div` factorial k) `div` factorial (n-k)

factorial :: Integer -> Integer
factorial n = (product :: [Integer] -> Integer) [1..n]

first  = head . unSeries
second = head . tail . unSeries

spec = do
  describe "convolutions" $ do
    it "test simple convolution" $ convolve [1,1] [1,1] `shouldBe` ([1,2,1] :: Series Int)
    prop "test convolution on pascal triangle" $ \(NonNegative row) ->
      (iterate (convolve [1,1]) [1])!!fromInteger row == pascalTriangle row
  it "test series addition" $ [0,2,3,4,5] + [1] `shouldBe` ([1,2,3,4,5] :: Series Int)
  it "test probability of coincidence (elementwise multiplication)" $ [0.2, 0.4, 0.4] .*. [0.25,0.0,0.25,0.25] `shouldBe` ([0.05,0.0,0.1] :: Series Double)
  describe "cumulative sums" $ do
    prop "cumulative sum on singleton is identity" $ \i ->
      cumsum [i] == [i::Int]
    it "simple cumulative sum" $
      cumsum [1,5,7] == [1,6,13]
    prop "cumulative sum of consecutive numbers" $ \(Positive n) ->
      last (unSeries $ cumsum [1..n]) == (n::Int)*(n+1) `div` 2
    prop "cumulative sum preserves input's first term" $ \s ->
      length (unSeries s) >= 1 ==>
        first (cumsum s) == first (s :: Series Int)
    prop "cumulative sum is left adjoint of discrete differences" $
      \s -> cumsum (diffEnc  s) == (s :: Series Integer)
    prop "cumulative sum after prepending zero is of left adjoint of discrete differences on non-null series" $
      \s -> length (unSeries s) > 0 ==> cumsum (diffEnc  (Series $ 0:unSeries s))
         == (Series $ (0::Int):unSeries s)
    prop "cumulative sum is of right adjoint of discrete differences" $
      \s -> diffEnc (cumsum s) == (s :: Series Int)
  describe "cut" $ do
    prop "length after cut is minimum of input length and series length" $
      \t s -> length (cut t s) == unDelay t `min` length (s :: Series Int)
    prop "length after cut is no longer than delay" $
      \t s -> length (cut t (s::Series Int)) <=  unDelay t
  describe "addition" $ do
    prop "scaling is distributive over fmap" $
      \a b (NonZero s) -> s.*(a+b) == (s.*a)+ (s.*(b ::Series Integer))
    prop "sum has the length of longest argument" $
        \a b -> size ((a::Series Int) + b) == max (size a) (size b)

size = length . unSeries

unDelay (Delay d) = d
