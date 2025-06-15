{-# LANGUAGE OverloadedStrings #-}

module Backtranspiler
  ( invertMap,
    backtranspile,
    transpileEvalBacktranspile,
    backtranspileFile,
    backtranspilePrint,
  )
where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Eval
import Parser
import Text.Parsec
import Transpiler

swap :: (b, a) -> (a, b)
swap (a, b) = (b, a)

invertMap :: (Ord k) => M.Map a k -> M.Map k a
invertMap m = M.fromList $ map swap $ M.toList m

backtranspile :: M.Map String SKI -> SKI -> String
backtranspile cDict (App ski1 ski2) = case M.lookup (App ski1 ski2) skiDict of
  Just colour -> colour
  Nothing -> case M.lookup ski2 skiDict of
    Just colour' -> colour' ++ backtranspile cDict ski1
    Nothing -> backtranspile (createColour cDict ski2) (App ski1 ski2)
  where
    skiDict = invertMap cDict
backtranspile cDict ski = M.findWithDefault "Can't transpile" ski skiDict
  where
    skiDict = invertMap cDict

-- | creates a colour with the given definition and inserts it to the colour dictionary
createColour :: M.Map [Char] p -> p -> M.Map [Char] p
createColour cDict def = go "Brown"
  where
    go colour = case M.lookup colour cDict of
      Nothing -> M.insert colour def cDict
      _ -> go (colour ++ "1")

-- | transpiles, evaluates and backtranspiles a Colourful expression
-- | via an intermediate SKI expression
transpileEvalBacktranspile :: M.Map String SKI -> T.Text -> Either ParseError String
transpileEvalBacktranspile cDict str = do
  (cmap, ski) <- parseInsert2SKIWMap cDict str
  return $ backtranspile cmap $ eval ski

-- backtranspiling functions for use in cli
--------------------------------------------

-- | reads file, parses, transpiles and evaluates program
-- | and transpiles back to Colourful
backtranspileFile :: FilePath -> IO ()
backtranspileFile filepath = do
  contents <- readFile filepath
  case parseInsert2SKIWMap colourDict (T.pack contents) of
    Left err -> putStrLn "Parse Error"
    Right (cmap, ski) -> print $ backtranspile cmap $ eval ski

-- | parses, transpiles and evaluates program
-- | and transpiles back to Colourful
backtranspilePrint :: String -> IO ()
backtranspilePrint str = do
  case parseInsert2SKIWMap colourDict (T.pack str) of
    Left err -> putStrLn "Parse Error"
    Right (cmap, ski) -> print $ backtranspile cmap $ eval ski
