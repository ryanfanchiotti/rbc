{-# LANGUAGE DataKinds #-}

module Main where

import qualified BC.Syntax as S
import qualified BC.Parser as P
import qualified BC.Analysis as A
import qualified BC.CodeGen.AMD64 as CGA
import qualified Text.Megaparsec as TMP
import Options.Declarative
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import System.Exit (die, exitSuccess)
import System.Process (callCommand)
import System.Directory
import System.IO

compiler :: Flag "o" '["output-file"] "a.out" "output location" (Def "a.out" String)
         -> Flag "S" '["emit-asm"] "false" "stop after assembly stage" Bool
         -> Flag "c" '["emit-obj"] "false" "stop after object stage" Bool
         -> Flag "s" '["static-link-libc"] "false" "link with the C runtime statically instead of dynamically" Bool
         -> Arg "input-file" String
         -> Cmd "Ryan's B Compiler" ()
compiler output emit_asm emit_obj static input = do
    liftIO $ compile (get output) (get emit_asm) (get emit_obj) (get static) (get input)

parse :: String -> IO S.Program
parse filename = do
    content <- readFile filename
    prog <- case TMP.parse P.pProg filename content of 
                Left bundle -> die $ TMP.errorBundlePretty bundle
                Right result -> return result
    return prog

analyze :: S.Program -> IO S.Program
analyze p = case A.analyzeProg p of
                Left err -> die err
                Right res -> return res

emit :: S.Program -> String -> IO ()
emit p output = let p_lines = CGA.emitProg p
                in writeFile output $ unlines p_lines

compile :: String -> Bool -> Bool -> Bool -> String -> IO ()
compile output emit_asm emit_obj static input = do
    ast <- parse input
    correct_ast <- analyze ast
    emit correct_ast output 
    when emit_asm exitSuccess
    commandWithTemp output "as" ""
    when emit_obj exitSuccess
    commandWithTemp output "gcc" (if static then " -static" else " -no-pie")

commandWithTemp :: FilePath -> String -> String -> IO ()
commandWithTemp output prog extra = do
    (temp_out, _) <- openTempFile "" "rbc.tmp"
    callCommand $ prog ++ " -o " ++ temp_out ++ " " ++ output ++ extra
    renameFile temp_out output

main :: IO ()
main = run_ compiler
