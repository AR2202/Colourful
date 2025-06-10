module Main (main) where

import Cli
import qualified Data.Map as M
import Transpiler (transpileFile)
import Eval (evalFile)


main :: IO ()
main = compilerCli commands

commands = M.fromList [
    ("test", const ( putStr "test")), 
    ("transpileREADME", const(transpileFile "README.md")),
    ("evalREADME", const (evalFile "README.md"))
    ]