module Main (main) where

import Cli
import qualified Data.Map as M


main :: IO ()
main = compilerCli commands

commands = M.fromList [("test", const ( putStr "test"))]