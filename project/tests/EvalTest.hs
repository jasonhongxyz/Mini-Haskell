module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck

import Ast
import Eval

import EnvUnsafeLog
--import Lang
--import EnvUnsafe

--import Lang

-- provide tests that show your run/eval works
{-
eqInt :: Val -> Val -> Bool
eqInt (I i) (I i') = i == i'
eqInt _ _ = False

eqFloat :: Val -> Val -> Bool
eqFloat (F f) (F f') = f == f'
eqFloat _ _ = False

eqBool :: Val -> Val -> Bool
eqBool (B b) (B b') = b == b'
eqBool _ _ = False

eqChar :: Val -> Val -> Bool
eqChar (C c) (C c') = c == c'
eqChar _ _ = False

eqString :: Val -> Val -> Bool
eqString (S s) (S s') = s == s'
eqString _ _ = False

eqList :: Val -> Val -> Bool
eqList (Ls (x:xs)) (Ls (y:ys)) = (x == y) && ((Ls xs) == (Ls ys))
eqList _ _ = False

eqFun :: Val -> Val -> Bool
eqFun (Fun f) (Fun g) = f == g
eqFun _ _ = False
-}

tests = testGroup "EvalTest"
  [
  testCase "Basic Ints: " $
    do
      assertEqual "Ints "        ((Ok $ I 2),[])   (run (ValInt 2))
      assertEqual "Ints "        ((Ok $ I (-10)),[]) (run (ValInt (-10)))
      assertEqual "Ints "        ((Ok $ I 0),[])   (run (ValInt (0))),
  testCase "Basic Bools: " $
    do
      assertEqual "Bool True "   ((Ok $ B True),[]) (run (ValBool True))
      assertEqual "Bool False "  ((Ok $ B True),[]) (run (ValBool True)),
  testCase "Basic Floats: " $
    do
      assertEqual "Floats "      ((Ok $ F 1.1),[])  (run (ValFloat 1.1))
      assertEqual "Floats "      ((Ok $ F (-3.14159)),[]) (run (ValFloat (-3.14159))),
  testCase "Basic Chars: " $
    do
      assertEqual "Chars "       ((Ok $ C 'a'),[])  (run (ValChar 'a'))
      assertEqual "Chars "       ((Ok $ C 'z'),[])  (run (ValChar 'z')),
  testCase "Basic Strings: " $
    do
      assertEqual "Strings "     ((Ok $ S "asdf"),[])  (run (ValString "asdf"))
      assertEqual "Strings "     ((Ok $ S "fe.d"),[])  (run (ValString "fe.d")),
  testCase "List Operators: " $
    do
      assertEqual
        "Lists [1,2]++[3,4] "
        ((Ok $ Ls [I 1, I 2, I 3, I 4]),[])
        (run (ListConcat (Cons (ValInt 1) (Cons (ValInt 2) Nil)) (Cons (ValInt 3) (Cons (ValInt 4) Nil))))
      assertEqual
        "[1,3] ++ 4"
        (Error "TypeMismatch: second argument (4) must be a list", [])
        (run (ListConcat (Cons (ValInt 1) (Cons (ValInt 3) Nil)) (ValInt 4)))
      assertEqual
        "4 ++ [1,3]"
        (Error "TypeMismatch: first argument (4) must be a list", [])
        (run (ListConcat (ValInt 4) (Cons (ValInt 1) (Cons (ValInt 3) Nil)))),
  testCase "Integer Arithmetic: " $
    do
      assertEqual "2 + 4 =? "    ((Ok $ I 6),[])    (run (Plus (ValInt 2) (ValInt 4)))
      assertEqual
        "2 + 4.0 =? "
        ((Error "TypeMismatch: Cannot add integer 2 and float 4.0"),[])
        (run (Plus (ValInt 2) (ValFloat 4.0)))
      assertEqual
        "2.0 + 4 =? "
        ((Error "TypeMismatch: Cannot add float 2.0 and integer 4"),[])
        (run (Plus (ValFloat 2.0) (ValInt 4)))
      assertEqual "2 - 4 =? "    ((Ok $ I (-2)),[]) (run (Minus (ValInt 2) (ValInt 4)))
      assertEqual
        "2 - 4.0 =? "
        ((Error "TypeMismatch: Cannot subtract integer 2 and float 4.0"),[])
        (run (Minus (ValInt 2) (ValFloat 4.0)))
      assertEqual
        "2.0 - 4 =? "
        ((Error "TypeMismatch: Cannot subtract float 2.0 and integer 4"),[])
        (run (Minus (ValFloat 2.0) (ValInt 4)))
      assertEqual "3 * 2 =? "    ((Ok $ I 6),[])    (run (Mult (ValInt 3) (ValInt 2)))
      assertEqual
        "2 * 4.0 =? "
        ((Error "TypeMismatch: Cannot multiply integer 2 and float 4.0"),[])
        (run (Mult (ValInt 2) (ValFloat 4.0)))
      assertEqual
        "2.0 * 4 =? "
        ((Error "TypeMismatch: Cannot multiply float 2.0 and integer 4"),[])
        (run (Mult (ValFloat 2.0) (ValInt 4)))
      assertEqual "3 // 2 =? "   ((Ok $ I 1),[])    (run (IntDiv (ValInt 3) (ValInt 2)))
      assertEqual
        "3.0 // 4 =? "
        ((Error "TypeMismatch: Can only use // with Integer types"),[])
        (run (IntDiv (ValFloat 3.0) (ValInt 4)))
      assertEqual
        "3 // 0 =? "
        ((Error "Error: Division-by-Zero"),[])
        (run (IntDiv (ValInt 3) (ValInt 0)))
      assertEqual "2 ** 4 =? "   ((Ok $ I 16),[])   (run (IntExp (ValInt 2) (ValInt 4)))
      assertEqual
        "3.0 ** 4 =? "
        ((Error "TypeMismatch: Can only use ** with Integer types"),[])
        (run (IntExp (ValFloat 3.0) (ValInt 4)))
      assertEqual "10 % 2 =? "   ((Ok $ I 0),[])    (run (Mod (ValInt 10) (ValInt 2)))
      assertEqual "10 % 3 =? "   ((Ok $ I 1),[])    (run (Mod (ValInt 10) (ValInt 3)))
      assertEqual
        "3.0 % 4 =? "
        ((Error "TypeMismatch: Can only use % with Integer types"),[])
        (run (Mod (ValFloat 3.0) (ValInt 4)))
      assertEqual
        "3 % 0 =? "
        ((Error "Error: Mod-by-Zero is undefined"),[])
        (run (Mod (ValInt 3) (ValInt 0))),
  testCase "Floating-Point Arithmetic: " $
    do
      assertEqual "2.3 + 4.1 =? "    ((Ok $ F 6.3999996),[])    (run (Plus (ValFloat 2.3) (ValFloat 4.1)))
      assertEqual "2.0 - 4.0 =? "    ((Ok $ F (-2.0)),[]) (run (Minus (ValFloat 2.0) (ValFloat 4.0)))
      assertEqual "3.0 * 2.0 =? "    ((Ok $ F 6.0),[])    (run (Mult (ValFloat 3.0) (ValFloat 2.0)))
      assertEqual "3.0 / 2.0 =? "    ((Ok $ F 1.5),[])    (run (FloatDiv (ValFloat 3.0) (ValFloat 2.0)))
      assertEqual
        "10 / 4.0 =? "
        ((Error "TypeMismatch: Can only use / with Float types"),[])
        (run (FloatDiv (ValInt 10) (ValFloat 4.0)))
      assertEqual
        "10.0 / 0.0 =? "
        ((Error "Error: Division-by-Zero"),[])
        (run (FloatDiv (ValFloat 10.0) (ValFloat 0.0)))
      assertEqual "2.0 ^ 4.0 =? "   ((Ok $ F 16.0),[])   (run (FloatExp (ValFloat 2.0) (ValFloat 4.0))),
  testCase "Comparison Operators: " $
    do
      assertEqual "3 == 3 =? "   ((Ok $ B True),[]) (run (Equals (ValInt 3) (ValInt 3)))
      assertEqual "3 != 4 =? "   ((Ok $ B True),[])  (run (NotEquals (ValInt 3) (ValInt 4)))
      assertEqual "4 < 5 =? "    ((Ok $ B True),[])  (run (LessThan (ValInt 4) (ValInt 5)))
      assertEqual
        "4 < 5.0 =? "
        ((Error "Types don't match. Can only compare values of same type."),[])
        (run (LessThan (ValInt 4) (ValFloat 5.0)))
      assertEqual "5 < 4 =? "    ((Ok $ B False),[]) (run (LessThan (ValInt 5) (ValInt 4)))
      assertEqual "4 > 5 =? "    ((Ok $ B False),[]) (run (GreaterThan (ValInt 4) (ValInt 5)))
      assertEqual
        "4 > 5.0 =? "
        ((Error "Types don't match. Can only compare values of same type."),[])
        (run (GreaterThan (ValInt 4) (ValFloat 5.0)))
      assertEqual "5 > 4 =? "    ((Ok $ B True),[])  (run (GreaterThan (ValInt 5) (ValInt 4)))
      assertEqual "4 <= 5 =? "   ((Ok $ B True),[])  (run (LessThanOrEquals (ValInt 4) (ValInt 5)))
      assertEqual
        "4 <= 5.0 =? "
        ((Error "Types don't match. Can only compare values of same type."),[])
        (run (LessThanOrEquals (ValInt 4) (ValFloat 5.0)))
      assertEqual "5 <= 4 =? "   ((Ok $ B False),[]) (run (LessThanOrEquals (ValInt 5) (ValInt 4)))
      assertEqual "4 >= 5 =? "   ((Ok $ B False),[]) (run (GreaterThanOrEquals (ValInt 4) (ValInt 5)))
      assertEqual
        "4 >= 5.0 =? "
        ((Error "Types don't match. Can only compare values of same type."),[])
        (run (GreaterThanOrEquals (ValInt 4) (ValFloat 5.0)))
      assertEqual "5 >= 4 =? "   ((Ok $ B True),[])  (run (GreaterThanOrEquals (ValInt 5) (ValInt 4)))
      assertEqual "5 >= 4 =? "   ((Ok $ B True),[])  (run (GreaterThanOrEquals (ValInt 5) (ValInt 4)))
      assertEqual "4 < 5.5=? "   (Error "Types don't match. Can only compare values of same type.",[]) (run (LessThan (ValInt 4) (ValFloat 5.5)))
      assertEqual "5.5 > 4=? "   (Error "Types don't match. Can only compare values of same type.",[]) (run (GreaterThan (ValFloat 5.5) (ValInt 4)))
      assertEqual "'a' <= 5=? "  (Error "Types don't match. Can only compare values of same type.",[]) (run (LessThanOrEquals (ValChar 'a') (ValInt 5)))
      assertEqual "'a' >= 5=? "  (Error "Types don't match. Can only compare values of same type.",[]) (run (GreaterThanOrEquals (ValChar 'a') (ValInt 5))),
  testCase "Boolean Operators: " $
    do
      assertEqual "True and True" ((Ok $ B True),[]) (run (And (ValBool True) (ValBool True)))
      assertEqual "True and False" ((Ok $ B False),[]) (run (And (ValBool True) (ValBool False)))
      assertEqual "False and True" ((Ok $ B False),[]) (run (And (ValBool False) (ValBool True)))
      assertEqual "False and False" ((Ok $ B False),[]) (run (And (ValBool False) (ValBool False)))
      assertEqual "True or True" ((Ok $ B True),[]) (run (Or (ValBool True) (ValBool True)))
      assertEqual "True or False" ((Ok $ B True),[]) (run (Or (ValBool True) (ValBool False)))
      assertEqual "False or True" ((Ok $ B True),[]) (run (Or (ValBool False) (ValBool True)))
      assertEqual "False or False" ((Ok $ B False),[]) (run (Or (ValBool False) (ValBool False)))
      assertEqual "True and 3"   (Error "3 is not a bool",[]) (run (And (ValBool True) (ValInt 3)))
      assertEqual "False or 0"  (Error "0 is not a bool",[]) (run (Or (ValBool False) (ValInt 0))),
  testCase "If Condition: " $
    do
      assertEqual "if True then 4 else 2 =? " ((Ok $ I 4),[]) (run (If (ValBool True) (ValInt 4) (ValInt 2)))
      assertEqual "if False then 1 else 4 =? " ((Ok $ I 4),[]) (run (If (ValBool False) (ValInt 1) (ValInt 4)))
      assertEqual "if 'a' then 4 else 2 =? " (Error "Condition ('a') must evaluate to a boolean",[]) (run (If (ValChar 'a') (ValInt 2) (ValInt 5))),
  testCase "Separator: " $
    do
      assertEqual "3;4 =? "    ((Ok $ I 4),[]) (run (Separator (ValInt 3) (ValInt 4)))
      assertEqual "2;10 =? "   ((Ok $ I 10),[]) (run (Separator (ValInt 2) (ValInt 10))),
  testCase "Let Statements: " $
    do
      assertEqual "let x = 4 in x * 2 =? " ((Ok $ I 8),[]) (run (Let ("x") (ValInt 4) (Mult (Var "x") (ValInt 2))))
      assertEqual "let x = 4 in x - 1 =? " ((Ok $ I 3),[]) (run (Let ("x") (ValInt 4) (Minus (Var "x") (ValInt 1))))
      assertEqual "let x = 3 in x and true" (Error "x is not a bool",[]) (run (Let ("x") (ValInt 3) (And (Var "x") (ValBool True)))),
  testCase "App/Lam Statements: " $
    do
      assertEqual "((lam)x -> x) 3" ((Ok $ I 3),[]) (run (App (Lam ("x") (Var "x")) (ValInt 3)))
      assertEqual "((lam)x -> x+3) true " (Error "x is not a number",[]) (run (App (Lam ("x") (Plus (Var "x") (ValInt 3))) (ValBool True)))
  ]

