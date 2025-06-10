module Eval(eval, 
evalFile,
evalPrint)
where
import Parser 
import Transpiler
import GHC.Generics (S)
import qualified Data.Text as T

-- | evaluation of SKI to a simpler SKI - not complete yet
eval :: SKI -> SKI
eval EmptyString = EmptyString
eval S = S
eval K = K
eval I = I 
eval (App I x) = eval x 
eval (App (App K x) _) = eval x
eval (App K x) = App K (eval x)
eval (App(App(App S x)y)z) = eval (App (App x z) (App y z))
eval (App (App S K )_) = I
eval (App S x) = App S (eval x)
eval (App(App S x) y) = App (App S (eval x)) (eval y)
eval (App x y) = eval (App (eval x) (eval y))

evalFile :: FilePath -> IO ()
evalFile filepath = do
  contents <- readFile filepath  
  case  parseAnd2SKIColourdict (T.pack contents) of 
    Left err -> putStrLn "Parse Error"
    Right ski -> print $ eval ski


evalPrint str = 
  case  parseAnd2SKIColourdict (T.pack str) of 
    Left err -> putStrLn "Parse Error"
    Right ski -> print $ eval ski