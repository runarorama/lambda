import Data.Maybe

-- Niave lambda calculus (i.e. no HOAS (I think)

-- not sure what role the environment should play except when 'externally' provided to eval, 
-- since without HOAS, we substitute (beta), rather extending the environment.
type Env = [(String, Term)]

data Term = Var String
          | App Term Term
          | Lam String Term
          | Lit Int
          -- built-ins
          | Add Term Term
          | Mul Term Term  deriving (Show)


eval :: Env -> Term -> Term                                   -- should eval just be Term -> Term?
eval e (Var s)     = eval e $ fromMaybe err (lookup s e)      
  where err = error $ "unknown var: " ++ s 

eval e (App t1 a) = case eval e t1 of
  Lam x b -> eval e $ beta x a b                              -- call by name
  _       -> error "term does not support application"

eval e l@(Lam s t) = alpha l                                  -- avoid variable capture

eval e (Lit i)     = Lit i

eval e (Add i1 i2) = let Lit l1 = eval e i1                   -- only Lit's can be added (error)
                         Lit l2 = eval e i2
                     in  Lit (l1 + l2)

eval e (Mul i1 i2) = let Lit l1 = eval e i1                   -- only Lit's can be multiplied (error)
                         Lit l2 = eval e i2
                     in  Lit (l1 * l2)


beta :: String -> Term -> Term -> Term                        -- suggests a functor instance?
beta x a v@(Var s)     = if s == x then a else v
beta x a   (App s t)   = App (beta x a s) (beta x a t)
beta x a l@(Lam s t)   = if s == x then l else Lam s (beta x a t)
beta x a   (Lit i)     = Lit i
beta x a   (Add i1 i2) = Add (beta x a i1) (beta x a i2) 
beta x a   (Mul i1 i2) = Mul (beta x a i1) (beta x a i2)

-- todo: impl alpha conversion
alpha x = x

test1 = eval [("x", Lit (-2))]
              (App (Lam "y" 
                      (Add (Var "x") 
                           (Var "y")))
                   (Lit 3))

test2 = eval [("y", Lit 1)]
              (App 
                (App 
                  (Lam "a" 
                    (Lam "b" (Var "a"))) 
                  (Var "y")) 
                (Lit 10)) 

-- capturing "y"
test3 = eval [("y", Lit 1)] 
              (App 
                (App 
                  (Lam "x" (Lam "y" (Var "x"))) 
                  (Var "y")) 
                (Lit 3))
