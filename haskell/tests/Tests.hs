module Main where

import LambdaCalculator

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

main :: IO ()
main = defaultMain [tests]


tests :: Test
tests = testGroup "Tests" [
    testCase "" testNaiveSimpleApp
  , testCase "" testNaiveCapturing
  , testCase "" testHOASSimpleApp
  , testCase "" testCapturingHOAS
  , testCase "" testClosureSimpleApp
  , testCase "" testCapturingClosure
  ]


simpleLit = Lit 1

id_ :: Term
id_ =  Lam "x" (Var "x")

simpleApp = App id_ (Lit 1)

const_ :: Term
const_ = Lam "x" (Lam "y" (Var "x"))


capturing = App (App const_ (Var "y")) (Lit 10)

testNaiveCapturing = assertEqual "Test failed"
                     (naiveEval [("y",Lit 42)] capturing)
                     (Lit 42)

testNaiveSimpleApp = assertEqual ""
                     (naiveEval [] simpleApp)
                     (Lit 1)

testHOASSimpleApp = assertEqual ""
                    (hoasEval [] simpleApp)
                    (Val 1)

testClosureSimpleApp = assertEqual ""
                       (closureEval [] simpleApp)
                       (Num 1)

testCapturingHOAS = assertEqual ""
                    (hoasEval [] capturing)
                    (Val 42)

testCapturingClosure = assertEqual ""
                       (closureEval [] capturing)
                       (Num 42)
