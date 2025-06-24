module Eval
  ( eval,
    evalFile,
    evalPrint,
  )
where

import qualified Data.Text as T
import GHC.Generics (S)
import Parser
import Transpiler

-- | evaluation of SKI to a simpler SKI - not complete yet
eval :: SKI -> SKI
eval EmptyString = EmptyString
eval S = S
eval K = K
eval I = I
eval (App I x) = eval x
eval (App (App K x) _) = eval x
eval (App K x) = App K (eval x)
eval (App (App (App S x) y) z) = eval (App (App x z) (App y z))
eval (App (App S K) _) = I
eval (App S x) = App S (eval x)
eval (App (App S x) y) = App (App S (eval x)) (eval y)
eval (App x y) = eval (App (eval x) (eval y))
-- functions for use in the CLI
------------------------------------
-- | transpiles the file at the provided filepath and evaluates the 
-- | SKI expression
evalFile :: FilePath -> IO ()
evalFile filepath = do
  contents <- readFile filepath
  case parseInsert2SKI colourDict (T.pack contents) of
    Left err -> putStrLn "Parse Error"
    Right ski -> print $ eval ski

-- | transpiles the input string and evaluates the SKI expression
evalPrint :: String -> IO ()
evalPrint str =
  case parseInsert2SKI colourDict (T.pack str) of
    Left err -> putStrLn "Parse Error"
    Right ski -> print $ eval ski
