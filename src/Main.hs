module Main where

import qualified Parser as P
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
-- main = parseTest (P.pExpr <* char ';') "var >>= 1 <<= 2 + 2 + 23;"
main = let 
            e = parse (P.pExpr <* char ';') "src" "4 >> 1;"
            printQ = putStrLn . show
       in case e of
            Right res -> printQ $ P.evalConstExpr res
            Left err -> putStr $ errorBundlePretty err
