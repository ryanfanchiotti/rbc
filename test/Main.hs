module Main (main) where

import Control.Applicative hiding (some)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Parser

type Parser = Parsec Void String

myParser :: Parser String
myParser = some (char 'a')

main :: IO ()
main = hspec $
  describe "myParser" $ do
    it "returns correct result" $
      parse myParser "" "aaa" `shouldParse` "aaa"
    it "result of parsing satisfies what it should" $
      parse myParser "" "aaaa" `parseSatisfies` ((== 4) . length)
    