module Main (main) where

import ParserTest
import Test.Hspec

main :: IO ()
main = hspec $ parseSpec
