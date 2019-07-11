module CheckTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Data.Set
import qualified Data.Set as Set
--import Test.Tasty.QuickCheck

import Ast
import Check

-- provide tests that show your check works

tests = testGroup "CheckTest"
  [
  testGroup "Check for Lambdas"
    [
      testCase "One free variable: " $
        do
          assertEqual
            "Lambda with one free var "
            (Set.singleton (UndefinedVarUse "y is not in scope"))
            (check (Lam "x" (Plus (Var "x") (Var "y")))),
      testCase "Multiple free variables: " $
        do
          assertEqual
            "Lambda with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope")])
            (check (Lam "x" (App (Var "z") (Var "y"))))
    ],
  testGroup "Check for let expressions"
    [
      testCase "One free variable: " $
        do
          assertEqual
            "Let expression with one free var "
            (Set.singleton (UndefinedVarUse "y is not in scope"))
            (check (Let "x" (ValInt 5) (Plus (Var "x") (Var "y")))),
      testCase "Multiple free variables: " $
        do
          assertEqual
            "Let expression with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope")])
            (check (Let "x" (ValInt 5) ((Var "x") `App` (Var "y") `App` (Var "z"))))
    ],
  testGroup "Check for Arithmetic expressions"
    [
      testCase "One free variable: " $
        do
          assertEqual
            "Plus expression with one free var "
            (Set.singleton (UndefinedVarUse "x is not in scope"))
            (check (Plus (Var "x") (ValInt 5)))
          assertEqual
            "Minus expression with one free var "
            (Set.singleton (UndefinedVarUse "x is not in scope"))
            (check (Minus (Var "x") (ValInt 5)))
          assertEqual
            "Mult expression with one free var "
            (Set.singleton (UndefinedVarUse "x is not in scope"))
            (check (Mult (Var "x") (ValInt 5)))
          assertEqual
            "IntDiv expression with one free var "
            (Set.singleton (UndefinedVarUse "x is not in scope"))
            (check (IntDiv (Var "x") (ValInt 5)))
          assertEqual
            "IntExp expression with one free var "
            (Set.singleton (UndefinedVarUse "x is not in scope"))
            (check (IntExp (Var "x") (ValInt 5))),
      testCase "Multiple free variables: " $
        do
          assertEqual
            "Plus expression with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope"),
                          (UndefinedVarUse "x is not in scope")])
            (check ((Var "x") `Plus` (Var "y") `Plus` (Var "z")))
          assertEqual
            "Minus expression with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope"),
                          (UndefinedVarUse "x is not in scope")])
            (check ((Var "x") `Minus` (Var "y") `Minus` (Var "z")))
          assertEqual
            "Mult expression with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope"),
                          (UndefinedVarUse "x is not in scope")])
            (check ((Var "x") `Mult` (Var "y") `Mult` (Var "z")))
          assertEqual
            "IntDiv expression with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope"),
                          (UndefinedVarUse "x is not in scope")])
            (check ((Var "x") `IntDiv` (Var "y") `IntDiv` (Var "z")))
          assertEqual
            "IntExp expression with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope"),
                          (UndefinedVarUse "x is not in scope")])
            (check ((Var "x") `IntExp` (Var "y") `IntExp` (Var "z")))
    ],
  testGroup "Check for Comparison expressions"
    [
      testCase "One free variable: " $
        do
          assertEqual
            "Equals expression with one free var "
            (Set.singleton (UndefinedVarUse "x is not in scope"))
            (check (Equals (Var "x") (ValInt 5)))
          assertEqual
            "NotEquals expression with one free var "
            (Set.singleton (UndefinedVarUse "x is not in scope"))
            (check (NotEquals (Var "x") (ValInt 5)))
          assertEqual
            "LessThan expression with one free var "
            (Set.singleton (UndefinedVarUse "x is not in scope"))
            (check (LessThan (Var "x") (ValInt 5)))
          assertEqual
            "GreaterThan expression with one free var "
            (Set.singleton (UndefinedVarUse "x is not in scope"))
            (check (GreaterThan (Var "x") (ValInt 5))),
      testCase "Multiple free variables: " $
        do
          assertEqual
            "Equals expression with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope"),
                          (UndefinedVarUse "x is not in scope")])
            (check ((Var "x") `Equals` (Var "y") `Equals` (Var "z")))
          assertEqual
            "NotEquals expression with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope"),
                          (UndefinedVarUse "x is not in scope")])
            (check ((Var "x") `NotEquals` (Var "y") `NotEquals` (Var "z")))
          assertEqual
            "LessThan expression with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope"),
                          (UndefinedVarUse "x is not in scope")])
            (check ((Var "x") `LessThan` (Var "y") `LessThan` (Var "z")))
          assertEqual
            "GreaterThan expression with multiple free vars "
            (Set.fromList [(UndefinedVarUse "y is not in scope"),
                          (UndefinedVarUse "z is not in scope"),
                          (UndefinedVarUse "x is not in scope")])
            (check ((Var "x") `GreaterThan` (Var "y") `GreaterThan` (Var "z")))
    ]
  ]

