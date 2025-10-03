{-# LANGUAGE OverloadedStrings #-}

module Transpiler
  ( parseAnd2SKI,
    parseAnd2SKIColourdict,
    insertDefs,
    transpileFile,
    transpilePrint,
    parseInsert2SKI,
    parseInsert2SKIWMap,
  )
where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Parser
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text

-- | transform into SKI
colour2SKI :: M.Map String a -> Colour -> Maybe a
colour2SKI m (ColourUse colour) = M.lookup colour m
colour2SKI _ _ = Nothing

-- | transforms a list of colours to a list of SKI expressions
colours2SKI :: M.Map String SKI -> [Colour] -> [SKI]
colours2SKI m colours =
  map fromJust $
    filter isJust $
      map (colour2SKI m) $
        colours

-- | parse and transpile to SKI - does not work on definitions
parseAnd2SKI :: M.Map String SKI -> T.Text -> Either ParseError SKI
parseAnd2SKI cDict t =
  buildAST
    <$> colours2SKI cDict
    <$> reverse
    <$> parseColours t

-- transforming a list of SKI into a SKI AST
buildAST :: [SKI] -> SKI
buildAST [] = EmptyString
buildAST [x] = x
buildAST xs = App (buildAST (init xs)) (last xs)

parseAnd2SKIColourdict :: T.Text -> Either ParseError SKI
parseAnd2SKIColourdict = parseAnd2SKI colourDict

-- inserting new colour
insertDef :: M.Map String SKI -> Colour -> M.Map String SKI
insertDef colourMap (ColourDef cname cdef) = case buildAST $
  colours2SKI colourMap $
    reverse cdef of
  EmptyString -> colourMap
  skis -> insertColour cname skis colourMap
insertDef m _ = m

-- insert all new colours
insertDefs :: M.Map String SKI -> [Colour] -> M.Map String SKI
insertDefs = L.foldl' insertDef

parseinsertDef :: (M.Map String SKI, [Colour]) -> T.Text -> (M.Map String SKI, [Colour])
parseinsertDef (colourMap, parsed) str =
  case parseColourdefWDict colourMap str of
    Right (ColourDef cname cdef) -> case buildAST $
      colours2SKI colourMap $
        reverse cdef of
      EmptyString -> (colourMap, parsed ++ [Comment])
      skis -> (insertColour cname skis colourMap, ColourDef cname cdef : parsed)
    Right x -> (colourMap, parsed ++ [x])
    Left _ -> case parseColoursWDict colourMap str of
      Right colours -> (colourMap, parsed ++ reverse colours)
      Left _ -> (colourMap, parsed ++ [Comment])

parseinsertDefs :: M.Map String SKI -> T.Text -> Either ParseError (M.Map String SKI, [Colour])
parseinsertDefs cmap str = L.foldl' parseinsertDef (cmap, []) . map T.pack <$> parse colourStringsParser "" str

-- | parses definitions and colour uses and transpiles to SKI
parseInsert2SKI :: M.Map String SKI -> T.Text -> Either ParseError SKI
parseInsert2SKI cmap str =
  buildAST
    <$> uncurry colours2SKI
    <$> parseinsertDefs cmap str

-- | like parseInsert2SKI, but returns the colour dictionary
parseInsert2SKIWMap ::
  M.Map String SKI ->
  T.Text ->
  Either ParseError (M.Map String SKI, SKI)
parseInsert2SKIWMap cmap str =
  let f (a, b) = (a, buildAST $ colours2SKI a b)
   in f <$> parseinsertDefs cmap str

-- transpiling functions for use in cli
--------------------------------------------
-- | reads the file at the provided path and transpiles it
transpileFile :: FilePath -> IO ()
transpileFile filepath = do
  contents <- readFile filepath
  case parseInsert2SKI colourDict (T.pack contents) of
    Left _ -> putStrLn "Parse Error"
    Right ski -> print ski

-- | transpiles the input string
transpilePrint :: String -> IO ()
transpilePrint str = do
  case parseInsert2SKI colourDict (T.pack str) of
    Left _ -> putStrLn "Parse Error"
    Right ski -> print ski
