module Lambda where

import Data.Set
import Data.Maybe

type Error = String

data Term = Var String
          | App Term Term
          | Lam Type String Term
          | Lit Int
          | Add Int Int
          | Mul Int Int
          | Ifz Term Term Term
          deriving (Show, Eq)

type Env = [(String, Term)]
          
eval :: Env -> Term -> Term
eval env i @ (Ifz it th el) = let it' = eval env it in
  case it' of
    (Lit 0) -> eval env th
    (Lit _) -> eval env el
    _ -> i

eval env (Add a1 a2) = Lit (a1 + a2)
eval env (Mul a1 a2) = Lit (a1 * a2)
eval env (Lit l) = Lit l
eval env (Lam t v b) = Lam t v $ eval env b
eval env (App t1 t2) = let t1' = eval env t1
                           t2' = eval env t2 in
                        case t1' of
                          Lam _ x t -> eval ((x, t2') : env) t 
                          _ -> error "can only apply to a lambda"

eval env (Var name) = lookup_var env name

lookup_var :: Env -> String -> Term
lookup_var [] n = Var n
lookup_var ((n1, val) : _) n2 | n1 == n2 = val
lookup_var (_ : xs) n = lookup_var xs n

rename :: Int -> Set String -> Term -> (Term,Int)
rename n bound (Var e) = if (member e bound) then ((Var e), n)
                         else ((Var (e ++ (show n))), n+1)
                           
rename n bound (App t1 t2) = let (t1',n') = rename n bound t1
                                 (t2',n'') = rename n' bound t2 in
                             ((App t1' t2'), n'')

rename n bound (Lam t s te) = let (t',n') = (rename n (insert s bound) te) in
  ((Lam t s t'), n')
rename n _ (Lit i) = ((Lit i),n)
rename n _ (Add i j) = ((Add i j), n)
rename n _ (Mul i j) = ((Mul i j), n)
rename n bound (Ifz i t1 t2) = let (t1', n') = (rename n bound t1)
                                   (t2', n'') = (rename n' bound t2) in
  ((Ifz i t1' t2'), n'')
                               

data Type = Int
          | Fun Type Type
          deriving (Show, Eq)


type TEnv = [(String, Type)]

typer :: TEnv -> Term -> Type
typer env (Var v) = fromMaybe (error $ "var " ++ v ++ " unknown") $ lookup v env
typer env (App t1 t2) = let t1t = typer env t1
                            t2t = typer env t2
                            in case (t1t,t2t) of
                          ((Fun a b), c) | a == c -> b
                          ((Fun a b), c) -> error $ "cannot apply type " ++ (show c) ++ " to a function of type " ++ (show a) ++ "=>" ++ (show b)
                          (a, b) -> error "can only apply to functions"
typer env (Lam t n te) = let t' = typer ((n,t) : env) te in
  Fun t t'
typer _ (Lit _) = Int
typer _ (Add _ _) = Int
typer _ (Mul _ _) = Int
typer env (Ifz t1 t2 t3) = let t1' = typer env t1
                               t2' = typer env t2
                               t3' = typer env t3 in
                            if(t2' == t3') then
                              if(t1' == Int) then t2' else error "IFz requires and Int"
                            else error "then and else expressions must have the same type"

ident = (Lam Int "x" (Var "x"))
first = (Lam Int "x" (Lam Int "y" (Var "x")))
second = (Lam Int "x" (Lam Int "y" (Var "y")))

assert :: String -> Bool -> IO () 
assert m b = putStrLn $ m ++ " - " ++ result where
  result = if b then "SUCCESS" else "FAILED"

test_eval :: IO ()
test_eval = do
  assert "fst (1,2) is 1" $ (eval [] (App (App first (Lit 1)) (Lit 2))) == (Lit 1)
  assert "snd (1,2) is 2" $ (eval [] (App (App second (Lit 1)) (Lit 2))) == (Lit 2)

test_typer :: IO ()
test_typer = do
  assert "identity is Int -> Int" $ (typer [] ident) == Fun Int Int
  assert "identity applied to one int is Int" $ (typer [] (App ident (Lit 1))) == Int
  assert "fst is Int -> Int -> Int" $ (typer [] first) == Fun Int (Fun Int Int)
  assert "snd is Int -> Int -> Int" $ (typer [] second) == Fun Int (Fun Int Int)
  assert "fst applied to one arg is Int -> Int" $ (typer [] (App second (Lit 1))) == Fun Int Int
  return ()

main :: IO ()
main = test_eval >> test_typer

