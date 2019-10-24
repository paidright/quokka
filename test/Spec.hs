module Main (
  main
) where

import Test.Hspec

import qualified Quokka.CreateSpec as CreateSpec


main :: IO ()
main =
  hspec CreateSpec.spec
