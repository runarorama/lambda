module Main where

import LambdaCalculator

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

main :: IO ()
main = defaultMain [tests]

tests :: Test
tests = testGroup "LambdaCalculator Test Expressions" [
    testCase "Simple literal eval" simpleLit
  , testCase "Simple variable assignment" simpleVar
  ]

simpleLit :: Assertion
simpleLit = assertEqual "Simple type lit" (Lit 2) (eval [] (Lit 2))

simpleVar :: Assertion
simpleVar = assertEqual "Simple variable"
            (Var "x" (Lit 10)) (eval [] $ Var "x" (Lit 10))
