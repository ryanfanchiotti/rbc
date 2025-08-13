module Main where

import qualified BC.Parser as P
import BC.Analysis
import qualified BC.CodeGen.AMD64 as A
import Text.Megaparsec
import System.Environment

escapeNewlines :: String -> String
escapeNewlines = foldr step ""
  where
    step '\n' acc = '\\':'n':acc
    step c acc = c:acc

main :: IO ()
main = do
     args <- getArgs
     (input, output) <- case args of
                    (f:o:_) -> return (f, o)
                    _ -> error "Input and output file not provided"
     content <- readFile input
     prog <- case parse P.pProg input content of 
          Left bundle -> do
               error (errorBundlePretty bundle)
          Right result -> do
               return result
     let prog_lines = A.emitProg prog
     let escaped_lines = map escapeNewlines prog_lines
     writeFile output (unlines escaped_lines)

