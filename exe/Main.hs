{-# LANGUAGE DataKinds #-}

module Main where

import qualified BC.Parser as P
import qualified BC.Analysis as A
import qualified BC.CodeGen.AMD64 as CGA
import Text.Megaparsec
import Options.Declarative
import Control.Monad.IO.Class (liftIO)

compile :: Flag "o" '["output-file"] "\"a.out\"" "output location" (Def "a.out" String)
        -> Flag "S" '["emit-asm"] "false" "stop after assembly stage" Bool
        -> Flag "c" '["emit-obj"] "false" "stop after object stage" Bool
        -> Flag "s" '["statically-link"] "false" "use static linking instead of dynamic linking" Bool
        -> Arg "input-file" String
        -> Cmd "Ryan's B Compiler" ()
compile output emit_asm emit_obj static input = do
    liftIO . putStrLn $ "output = " ++ get output 
    liftIO . putStrLn $ "emit_asm = " ++ (show $ get emit_asm)
    liftIO . putStrLn $ "emit_obj = " ++ (show $ get emit_obj)
    liftIO . putStrLn $ "static = " ++ (show $ get static)
    liftIO . putStrLn $ "input = " ++ get input

main :: IO ()
main = run_ compile
