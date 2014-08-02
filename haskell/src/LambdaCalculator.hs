module LambdaCalculator where

type Name = String

data Term = Lit Int
          | Var Name
          | App Term Term
          | Lam Name Term
          deriving (Eq, Show)


------------------------------------------------------------------------------
type NaiveEnv = [(Name, Term)]

naiveEval :: NaiveEnv -> Term -> Term
naiveEval = undefined


------------------------------------------------------------------------------
data HOAS = Val Int
          | Fun (HOAS -> HOAS)

instance Show HOAS where
  show (Val i) = "Val " ++ show i
  show (Fun _) = "A function"

instance Eq HOAS where
  (Val i) == (Val j) = i == j
  _ == _  = error "Don't know"

type HoasEnv = [(Name, HOAS)]


hoasEval :: HoasEnv -> Term -> HOAS
hoasEval = undefined


------------------------------------------------------------------------------
data ClosureRuntime = Num Int
                    | Closure Name Term ClosureEnv
                    deriving (Show, Eq)


type ClosureEnv = [(Name, ClosureRuntime)]

closureEval :: ClosureEnv -> Term -> ClosureRuntime
closureEval = undefined
