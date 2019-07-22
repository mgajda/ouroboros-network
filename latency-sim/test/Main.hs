-- file test/Main.hs
module Main where

import Test.Hspec(parallel)
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Discovered(spec)

main :: IO ()
--main = hspecWith defaultConfig {configFormatter = Just progress} Spec.spe
main = hspecWith defaultConfig $ parallel $ Discovered.spec
