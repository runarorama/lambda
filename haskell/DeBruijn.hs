module Lamb where

import Control.Applicative
import Data.List (elemIndex)

-- A simply-typed lambda calculator with let-bindings.
-- Implementation uses de Bruijn indices rather than a binding environment.
-- Rob Norris / @tpolecat

-- Identifiers are strings
type Ident = String

-- Types
data Type
  = TInt
  | TFun Type Type
  deriving (Show, Eq)

-- Surface syntax
data Surface
  = SVar Ident
  | SApp Surface Surface
  | SLam Type Ident Surface
  | SNum Int
  | SAdd Surface Surface
  | SSub Surface Surface
  | SIfZ Surface Surface Surface
  | SLet [(Ident, Type, Surface)] Surface
  deriving Show

-- Desugared, with de Bruijn indices, expanded let-bindings, and generalized
-- binary operators.
data Desugared
  = DBound Int
  | DApp Desugared Desugared
  | DLam Type Desugared
  | DNum Int
  | DBin (Int -> Int -> Int) Desugared Desugared
  | DIfZ Desugared Desugared Desugared

-- The function in DBin means we have to do this by hand
instance Show Desugared where
  show d = "(" ++ s ++ ")" where 
    s = case d of 
      DBound n   -> "DBound "     ++ show n
      DNum n     -> "DNum "       ++ show n
      DLam t a   -> "DLam "       ++ show t ++ " " ++ show a
      DApp a b   -> "DApp "       ++ show a ++ " " ++ show b
      DBin _ a b -> "DBin <fun> " ++ show a ++ " " ++ show b
      DIfZ b t f -> "DIfZ "       ++ show b ++ " " ++ show t ++ " " ++ show f

-- We compute a value
data Value
  = VFun [Value] Desugared -- a closure
  | VNum Int
  deriving Show

-- But things can go wrong
data Error
  = Unbound  Ident
  | NotAFun  Desugared Type -- element, type
  | IllTyped Desugared Type Type -- element expected actual
  | DuplicateBinding Ident 
  deriving Show

-- Why is this not in stdlib?
toEither :: b -> Maybe a -> Either b a
toEither b = maybe (Left b) Right

-- Desugar and replace all bound variables with de Bruijn indices, or
-- report an error if an unbound variable is encountered. Desugar let blocks
-- into a series of lambdas, disallowing duplicates.
desugar :: [Ident] -> Surface -> Either Error Desugared
desugar e s = case s of
  SNum n     -> Right (DNum n)
  SVar i     -> DBound   <$> toEither (Unbound i) (elemIndex i e) 
  SLam t i a -> DLam t   <$> desugar (i : e) a
  SApp a b   -> DApp     <$> desugar e a <*> desugar e b
  SAdd a b   -> DBin (+) <$> desugar e a <*> desugar e b
  SSub a b   -> DBin (-) <$> desugar e a <*> desugar e b
  SIfZ b t f -> DIfZ     <$> desugar e b <*> desugar e t <*> desugar e f
  SLet bs a  -> checkBindings bs >> desugar e (foldl expand a bs) where 
    expand a (i, t, s) = (SApp (SLam t i a) s)
    checkBindings [] = Right ()
    checkBindings ((i, _, _) : bs) = 
      if (any (\(i', _, _) -> i == i') bs) 
        then Left (DuplicateBinding i) 
        else checkBindings bs

-- Typechecker. With de Bruijn indices our type environment is simply a stack. 
typecheck :: [Type] -> Desugared -> Either Error Type
typecheck e d = case d of
  DNum n     -> Right TInt
  DBound n   -> Right (e !! n) -- safe due to desugaring
  DLam t a   -> TFun t <$> typecheck (t : e) a
  DBin _ a b -> 
    do ta <- typecheck e a
       tb <- typecheck e b
       case (ta, tb) of 
         (TInt, TInt) -> Right TInt
         (TInt, tb)   -> Left (IllTyped b TInt tb)
         (ta, _)      -> Left (IllTyped a TInt ta)
  DApp a b -> 
    do ta <- typecheck e a
       tb <- typecheck e b
       case ta of
         TFun x y | x == tb   -> Right y
         TFun x y | otherwise -> Left (IllTyped b x tb)
         x                    -> Left (NotAFun b x)
  DIfZ b t f ->
    do tb <- typecheck e b
       tt <- typecheck e t
       tf <- typecheck e f
       case tb of 
         TInt | tt == tf  -> Right tt
         TInt | otherwise -> Left (IllTyped f tt tf)
         x                -> Left (IllTyped b TInt tb)

-- Evaluate a typechecked program, yielding a value or an error.
-- With de Bruijn indices our binding environment is simply a stack. 
eval :: [Value] -> Desugared -> Either Error Value
eval e d = case d of
  DNum n   -> Right (VNum n)
  DBound n -> Right (e !! n) -- safe due to desugaring
  DLam _ a -> Right (VFun e a)
  DBin f a b -> 
    do VNum a <- eval e a -- safe due to typechecking
       VNum b <- eval e b
       Right $ VNum (f a b) 
  DApp a b -> 
    do VFun e a <- eval e a -- safe due to typechecking
       b <- eval e b
       eval (b : e) a 
  DIfZ b t f ->
    do VNum a <- eval e b
       eval e $ if (a == 0) then t else f

-- Desugar and evaluate a program in surface syntax
run :: Surface -> Either Error (Type, Value)
run p = do d <- desugar [] p 
           t <- typecheck [] d
           v <- eval [] d
           return (t, v)

-- Some example programs

xid = SLam TInt "a" (SVar "a") -- id
xconst = SLam TInt "a" $ SLam TInt "b" (SVar "a") -- const
xinc = SLam TInt "a" $ SAdd (SVar "a") (SNum 1) -- (\a -> a + 1)
xdec = SLam TInt "a" $ SSub (SVar "a") (SNum 1)  -- (\a -> a - 1)

xerr1 = SLam TInt "a" $ SLam TInt "b" (SVar "c") -- unbound
xerr2 = SApp (SNum 3) (SNum 3) -- not a function
xerr3 = SAdd xid xid -- not a number

xtest1 = SApp (SLam TInt "a" $ SAdd (SApp (SLam TInt "b" (SVar "a")) (SNum 2)) (SVar "a")) (SNum 3) -- (\a -> ((\b -> a) 2) + a) 3 --> 6
xtest2 = SApp (SLam (TFun TInt TInt) "f" (SApp (SVar "f") (SNum 1))) xid -- (\f -> f 1) id --> 1
xtest3 = SApp (SLam (TFun TInt TInt) "f" (SApp (SVar "f") (SNum 1))) (SApp xconst (SNum 10)) -- (\f -> f 1) (const 10) --> 10






