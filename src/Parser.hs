{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parsea,
    parseoneof,
    parseColour,
    parseColours,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text

-- Just playing
data Ex = A | B | Oneof String deriving (Show, Read, Eq)

aparser :: Parser Ex
aparser = A <$ string "Aa"

parsea :: T.Text -> Either ParseError Ex
parsea = parse aparser ""

parseoneof :: T.Text -> Either ParseError Ex
parseoneof = parse oneParser ""

oneParser :: Parser Ex
oneParser = Oneof <$> choice (map string ["A", "B"])

-- implementation
data Colour = ColourUse String | ColourDef String Colour | Comment deriving (Show, Read, Eq)

data SKI = S | K | I | App SKI SKI deriving (Show, Read, Eq)

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

colourParser :: Parser Colour
colourParser = ColourUse <$> choice (map string $ M.keys colourDict)

coloursParser :: Parser [Colour]
coloursParser = many (spaces *> colourParser <* spaces)

-- | parse any colour
parseColour :: T.Text -> Either ParseError Colour
parseColour = parse colourParser ""

-- | parse many matches of a colour into a list
parseColours :: T.Text -> Either ParseError [Colour]
parseColours = parse coloursParser ""
