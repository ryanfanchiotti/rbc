module Main (main) where

import ParserTest
import AnalysisTest
import Test.Hspec

main :: IO ()
main = hspec $ do
           parseSpec
           analysisSpec
