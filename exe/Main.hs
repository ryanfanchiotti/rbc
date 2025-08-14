module Main where

import qualified BC.Parser as P
import BC.Analysis
import qualified BC.CodeGen.AMD64 as A
import Text.Megaparsec
import System.Environment

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
     writeFile output (unlines prog_lines)

