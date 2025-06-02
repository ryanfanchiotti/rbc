module Main where

import qualified Parser as P
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = parseTest (P.pExpr <* char ';') "var >>= 1;"
-- main = let 
--             e = parse (P.pExpr <* char ';') "src" "(1 + (200 * 3) / 2 / 2 >= 150 + 2) - 1 - 10000 < 10000000;"
--             printQ = putStrLn . show
--        in case e of
--             Right res -> printQ $ P.evalConstExpr res
--             Left err -> putStr $ errorBundlePretty err
