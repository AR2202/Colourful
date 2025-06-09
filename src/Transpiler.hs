{-# LANGUAGE OverloadedStrings #-}

module Transpiler
  ( parseAnd2SKI,
    parseAnd2SKIColourdict,
    insertDefs,
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

colours2SKI :: M.Map String SKI -> [Colour] -> [SKI]
colours2SKI m colours =
  map fromJust $
    filter isJust $
      map (colour2SKI m) $
        colours

parseAnd2SKI :: M.Map String SKI -> T.Text -> Either ParseError SKI
parseAnd2SKI cDict t = buildAST <$> colours2SKI cDict <$> reverse <$> parseColours t

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
