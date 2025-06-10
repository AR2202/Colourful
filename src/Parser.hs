{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseColour,
    parseColours,
    parseComment,
    insertBrown,
    insertColour,
    Colour (..),
    SKI (..),
    colourDict,
    parseColourdefWDict,
    parseBW,
    isColourUse
  )
where

import Control.Monad (guard, void)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import GHC.OldList (isInfixOf)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text

-- Colour Parser
data Colour = ColourUse String | ColourDef String [Colour] | Comment deriving (Show, Read, Eq)

data SKI = EmptyString | S | K | I | App SKI SKI deriving (Read, Eq)

instance Show SKI where
  show EmptyString = ""
  show S = "S"
  show K = "K"
  show I = "I"
  show (App x S) = show x ++ "S"
  show (App x K) = show x ++ "K"
  show (App x I) = show x ++ "I"
  show (App x EmptyString) = show x
  show (App x y) = show x ++ "(" ++ show y ++ ")"

colourDict :: M.Map String SKI
colourDict =
  M.fromList
    [ ("Red", K),
      ("Yellow", I),
      ("Blue", S),
      ("Orange", App K I),
      ("Purple", App K S),
      ("Pink", App K K),
      ("Cyan", App S S),
      ("Violet", App S K),
      ("Green", App S I),
      ("Lime", App (App S I) I),
      ("Teal", App (App S I) S)
    ]

-- | helper function to insert a colour into the map
insertColour :: String -> SKI -> M.Map String SKI -> M.Map String SKI
insertColour = M.insertWith (flip const)

-- helper functions

-- | helper function to check for Comments
isComment :: Colour -> Bool
isComment Comment = True
isComment _ = False

-- | helper function to check for Colour definitions
isColourDef :: Colour -> Bool
isColourDef (ColourDef _ _) = True
isColourDef _ = False

-- | helper function to check for Colour Use
isColourUse :: Colour -> Bool
isColourUse (ColourUse _) = True
isColourUse _ = False

-- Parsers
-------------
commentParser :: Parser Colour
commentParser = Comment <$ anyChar

commentParser' :: Parser Colour
commentParser' = Comment <$ (notFollowedBy (many1 alphaNum *> spaces *> string "White") *> anyChar)

colourParser :: Parser Colour
colourParser = ColourUse <$> choice (map string $ M.keys colourDict)

coloursParser :: Parser [Colour]
coloursParser = many (spaces *> (try colourParser <|> commentParser) <* spaces)

-- | Parses a colour; like colourParser, but takes the dictionary of colours as an argument
colourParserWDict :: M.Map String SKI -> Parser Colour
colourParserWDict cDict = ColourUse <$> choice (map string $ M.keys cDict)

-- | Parses many colours; like coloursParser, but takes the dictionary of colours as an argument
coloursParserWDict :: M.Map String SKI -> Parser [Colour]
coloursParserWDict cDict = many (spaces *> (try (colourParserWDict cDict) <|> commentParser) <* spaces)

-- | Parses many colours; like coloursParser, but takes the dictionary of colours as an argument
coloursParserWDict' :: M.Map String SKI -> Parser [Colour]
coloursParserWDict' cDict = many (spaces *> (try (colourParserWDict cDict) <|> try commentParser') <* spaces)

-- | helper for parsing the colourDef variant of Colour
colourNameDefParser :: M.Map String SKI -> Parser Colour
colourNameDefParser cDict = flip ColourDef <$> coloursParserWDict' cDict <*> (many1 alphaNum <* spaces)

-- | Parses the colourDef variant of Colour
colourDefParser :: M.Map String SKI -> Parser Colour
colourDefParser cDict = between (string "Black") (string "White") (colourNameDefParser cDict)

-- | parsing out the colour definition Strings
blackWhiteParser :: Parser String
blackWhiteParser =
  do
    _ <- string "Black"
    content <- manyTill anyChar (try (string "White"))
    guard (not ("Black" `isInfixOf` content))

    return $ "Black" ++ content ++ "White"

-- parsing
--------------

-- | parse any colour
parseColour :: T.Text -> Either ParseError Colour
parseColour = parse colourParser ""

-- | parse many matches of a colour into a list
parseColours :: T.Text -> Either ParseError [Colour]
parseColours = parse coloursParser ""

-- | parse many matches of a colour into a list
parseColoursWDict :: M.Map String SKI -> T.Text -> Either ParseError [Colour]
parseColoursWDict cDict = parse (coloursParserWDict cDict) ""

-- | parse any char
parseComment :: T.Text -> Either ParseError Colour
parseComment = parse commentParser ""

-- | parse colourdef
parseColourdefWDict :: M.Map String SKI -> T.Text -> Either ParseError Colour
parseColourdefWDict cDict = parse (colourDefParser cDict) ""

-- | parse blackwhite
parseBW = parse blackWhiteParser ""

-- testing
insertBrown :: M.Map String SKI
insertBrown = insertColour "Brown" K colourDict
