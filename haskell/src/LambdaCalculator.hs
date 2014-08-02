module LambdaCalculator where

import Data.List

data Term = Lit Int
          | Var String Term
          | App Term   Term
          | Lam String Term
          deriving (Eq, Show)

type Env = [(String, Term)]

eval :: Env -> Term -> Term
eval _ (Lit a)     = Lit a
eval e (Var n t)   = maybe undefined (\v -> eval e v) (lookup n e)
eval e (App f@(Lam s t) (Var vs vv)) = eval ((vs, vv):e) f
