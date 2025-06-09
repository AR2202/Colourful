module Eval(eval)
where
import Parser 
import GHC.Generics (S)

eval :: SKI -> SKI
eval EmptyString = EmptyString
eval S = S
eval K = K
eval I = I 
eval (App I x) = eval x 
eval (App K x) = App K (eval x)
eval (App(App(App S x)y)z) = eval (App (App x z) (App y z))
eval (App (App S K )_) = I
eval (App(App S x) y) = App (App S (eval x)) (eval y)
eval (App x y) = App (eval x) (eval y)