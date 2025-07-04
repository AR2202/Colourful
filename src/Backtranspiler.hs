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

-- | transpile an SKI expression back to a Colourful expression string
-- backtranspile :: M.Map String SKI -> SKI -> String
-- backtranspile cDict ski = snd $ backtranspile' cDict ski ""
backtranspile :: M.Map [Char] SKI -> SKI -> [Char]
backtranspile cDict ski = makeBacktranspileString fullDict ski ++ makeDefs fullDict newDefs 
  where fullDict = makeBacktranspileDict cDict ski 
        newDefs = M.difference fullDict cDict
-- | insert all required additional definitions into the dictionary
makeBacktranspileDict :: M.Map [Char] SKI -> SKI -> M.Map [Char] SKI
makeBacktranspileDict  cDict (App ski1 ski2) = case M.lookup (App ski1 ski2) skiDict of
  Just _ -> cDict

  Nothing -> case M.lookup ski2 skiDict of
    Just _ -> makeBacktranspileDict cDict ski1 
    Nothing -> makeBacktranspileDict (createColour ( makeBacktranspileDict cDict ski2) ski2) (App ski1 ski2)
  where
    skiDict = invertMap cDict
makeBacktranspileDict cDict _  = cDict

-- | creating the string, assuming the dictionary contains all definitions
makeBacktranspileString :: M.Map [Char] SKI -> SKI -> [Char]
makeBacktranspileString cDict (App ski1 ski2)  = case M.lookup (App ski1 ski2) skiDict of
  Just colour -> colour 
  Nothing -> case M.lookup ski2 skiDict of
    Just colour' ->  colour' ++ makeBacktranspileString cDict ski1 
    Nothing ->  "error make string" -- this should not happen
  where
    skiDict = invertMap cDict
makeBacktranspileString cDict ski = M.findWithDefault "error skiNot there" ski skiDict 
  where
    skiDict = invertMap cDict
-- | make a colour definition, given the colour and a dictionary
-- | containing the definition
-- this is using a default value for missing values 
-- TODO improve error handling
makeDef :: M.Map [Char] SKI -> [Char] -> [Char]
makeDef cDict colour = "Black " ++ makeBacktranspileString (M.delete colour cDict) (M.findWithDefault I colour cDict) ++ " " ++ colour ++ " White"


makeDefs :: M.Map [Char] SKI -> M.Map [Char] a -> [Char]
makeDefs cDict newDefs = concatMap (makeDef cDict) (reverse $ M.keys newDefs)
-- | attempt at fixing backtranspiler - not quite working
backtranspile' :: M.Map String SKI -> SKI -> [Char] -> (M.Map String SKI, [Char])
backtranspile' cDict (App ski1 ski2) defs = case M.lookup (App ski1 ski2) skiDict of
  Just colour -> (cDict, colour ++ defs)
  Nothing -> case M.lookup ski2 skiDict of
    Just colour' -> (fst(backtranspile' cDict ski1 defs), colour' ++ snd (backtranspile' cDict ski1 defs))
    Nothing -> (colDict, snd (backtranspile' colDict (App ski1 ski2) (defs ++ defstr)))
  where
    skiDict = invertMap cDict
    (defstr, colDict) = createColourWDef cDict ski2
backtranspile' cDict ski _ = (cDict, M.findWithDefault "Can't transpile" ski skiDict)
  where
    skiDict = invertMap cDict

-- | creates a colour with the given definition and inserts it to the colour dictionary
createColour :: M.Map [Char] p -> p -> M.Map [Char] p
createColour cDict def = go "Brown"
  where
    go colour = case M.lookup colour cDict of
      Nothing -> M.insert colour def cDict
      _ -> go (colour ++ "1")

-- | creates a colour with the given definition,
-- |returns a string of the definition
-- | and inserts it to the colour dictionary
-- createColourWDef :: M.Map [Char] p -> p -> M.Map [Char] p
createColourWDef :: M.Map String SKI -> SKI -> ([Char], M.Map String SKI)
createColourWDef cDict def = go "Brown"
  where
    go colour = case M.lookup colour cDict of
      Nothing -> (defstr colour, M.insert colour def (newCdict colour))
      _ -> go (colour ++ "1")
    -- this is not quite correct yet, as it will nest definitions
    -- rather than chaining them
    -- but it will work if only one additional definition is needed
    defstr colour = "Black " ++ snd (backtranspile' (M.insert colour EmptyString cDict) def "") ++ " " ++ colour ++ " White"
    newCdict colour = fst (backtranspile' (M.insert colour EmptyString cDict) def "")
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
