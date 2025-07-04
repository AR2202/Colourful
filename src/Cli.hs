module Cli (compilerCli) where

import qualified Data.Map as M
import Eval
import Parser
import Transpiler

compilerCli :: M.Map String ([String] -> IO a) -> IO ()
compilerCli commands = do
  putStrLn "please enter command or quit to exit"
  inp <- getLine

  let inpWords = words inp
  case inpWords of
    [] -> compilerCli commands
    ("quit" : _) -> putStrLn "goodbye!"
    (x : xs) -> case M.lookup x commands of
      Nothing -> putStrLn (x ++ " is not a valid command") >> compilerCli commands
      Just f -> f xs >> compilerCli commands
