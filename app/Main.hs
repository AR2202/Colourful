module Main (main) where

import Cli
import qualified Data.Map as M
import Eval (evalFile, evalPrint)
import Transpiler (transpileFile, transpilePrint)
import Backtranspiler  (backtranspileFile, backtranspilePrint)

main :: IO ()
main = compilerCli commands

commands :: M.Map String ([FilePath] -> IO ())
commands =
  M.fromList
    [ ("test", const (putStr "test")),
      ("transpileREADME", const (transpileFile "README.md")),
      ("evalREADME", const (evalFile "README.md")),
      ("transpileFile", checkArgsFile transpileFile),
      ("evalFile", checkArgsFile evalFile),
      ("backtranspileFile", checkArgsFile backtranspileFile),
      ("transpile", checkArgsPrint transpilePrint),
      ("eval", checkArgsPrint evalPrint),
      ("backtranspile", checkArgsPrint backtranspilePrint)

    ]

checkArgsFile :: (t -> IO ()) -> [t] -> IO ()
checkArgsFile _ []  =
  putStrLn "not enough arguments - please provide filepath"
checkArgsFile f (x : xs) = f x



checkArgsPrint :: (String -> IO ()) -> [String] -> IO ()
checkArgsPrint f [] =
  putStrLn "not enough arguments - please provide input string"
checkArgsPrint f xs = f $ unwords xs


