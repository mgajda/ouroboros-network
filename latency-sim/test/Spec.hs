{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Main(main) where

import Latency

import Test.QuickCheck
import Test.QuickCheck.All
import Test.Hspec
import Test.Hspec.QuickCheck

test_earliestApproximatesTTC = property earliestApproximatesTTC

test_latestApproximatesTTC   = property latestApproximatesTTC

skip _ = pure ()

main :: IO ()
main = hspec $ do
         describe "ops demo" $ do
           it "tests" $ do
             2+2 `shouldBe` 4
           it "earliest of noDelay is 0" $ do
             earliest [1] == Earliest 0
           skip $ it "earliest of allLost is 0" $ do
             earliest allLostLD == Earliest 0
           it "earliest of delay t is t" $ do
             property $ \t -> earliest (delay t) == Earliest (t :: Delay)
         describe "quickcheck" $ do
           it "earliest approximates latency distribution" $ test_earliestApproximatesTTC
           skip $ it "earliest approximates latency distribution" $ test_latestApproximatesTTC

