import Data.Maybe
import Data.List
import Control.Monad

type Name = String

data Term
  = Var Name             -- Variables
  | Lit Int              -- Literals
  | App Term Term        -- Function application
  | Lam Name Type Term   -- Lambda abstraction
  deriving (Show, Eq, Ord)

data Type = Int | Fun Type Type deriving (Show, Eq, Ord)

-- Runtime values
data Runtime = Value Int | Function (Runtime -> Runtime)

instance Show Runtime where
  show (Value n) = show n
  show (Function f) = "<function>"

-- The runtime environment
type Env = [(Name, Runtime)]

-- Evaluate a term to a runtime value given an environment
eval :: Env -> Term -> Runtime
eval env (Var s) = fromMaybe (error $ "Unbound variable: " ++ s) $ lookup s env
eval env (Lam x _ e) = Function (\v -> eval ((x,v) : env) e)
eval env (App e1 e2) = case eval env e1 of
  Function f -> f (eval env e2)
  Value v    -> error $ "Not a function: " ++ show v
eval _ (Lit n) = Value n

i = Lam "x" Int (Var "x")
k = Lam "x" Int (Lam "y" Int (Var "x"))

capturing = App (App k (Var "y")) (Lit 10)
noncapturing= App (App (Lam "a" Int (Lam "b" Int (Var "a"))) (Var "y")) (Lit 10)

empty = []
wy = [("y", Value 42)]

-- Typer

type TEnv = [(Name, Type)]

typer :: TEnv -> Term -> Type
typer _ (Lit _) = Int
typer env (Var v) = fromMaybe (error $ "Unbound variable " ++ v) $ lookup v env
typer env (App e1 e2) = let t1 = (typer env e1) in case t1 of
  (Fun ta tr) -> let t2 = typer env e2 in
    if (ta == t2) then tr
    else error $ "Type mismatch. Expected " ++
                 show ta ++ " found " ++ show t2
  _ -> error $ "Not a function type: " ++ show t1
typer env (Lam x t e) = Fun t (typer ((x,t):env) e)

