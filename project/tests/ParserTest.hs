module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck

--import Lang (showFullyParen, showPretty, Ast(..))
import ParserMonad (parse)
import Ast
import Parser
import Eval
import HelpShow
-- provide tests that show your parser works

  
instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i    <- arbitrary -- will choose a random Integer
                                 b    <- arbitrary
                                 node <- elements [Nil, ValInt (abs i), ValBool b]  -- so put all the non-recursive Ast expressions here
                                 return $ node
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)  -- get ast half as big
                                     r <- arbitrarySizedAst (m `div` 2)  -- ditto
                                     q <- elements [Nil, Cons l Nil]
                                     x <- elements ["x", "y", "z"]   -- will choose random element from the list
                                     ifAst <- arbitrarySizedIf m
                                     node <- elements [And l r, Or l r, Not l,
                                                       Plus l r, Minus l r, Mult l r, IntDiv l r, FloatDiv l r,
                                                       Equals l r, NotEquals l r, LessThan l r, GreaterThan l r, LessThanOrEquals l r, GreaterThanOrEquals l r,

                                                       IntExp l r, FloatExp l r,


                                                       Cons l Nil,
                                                       ListIndex l r, ifAst,
                                                       Let x l r, App l r, Lam x l, NegExp l
                                                      ]
                                     return node

-- break in thirds for mix-fix operators which have three separate sub-asts

e1 = showPretty (Minus (ValInt 100) (Minus (ValInt 2) (ValInt 5))) $  0
e2 = showPretty (Minus (Minus (ValInt 100) (ValInt 2)) (ValInt 5) ) $  0

e3 = showPretty (Minus (Minus (ValInt 100) (ValInt 2)) (IntDiv (IntDiv (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0
e4 = showPretty (IntDiv (Minus (ValInt 100) (ValInt 2)) (IntDiv (IntDiv (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0
e5 = showPretty (((Var "fun") `App` (ValInt 2)) `App` (ValInt 5)) $ 0

e6 = showPretty (Not $ Not $ ((Var "fun") `App` (ValInt 2)) `App` (Not $ ValInt 5)) $ 0
e7 = showPretty (Equals (Minus (ValInt 100) (ValInt 2)) (IntDiv (IntDiv (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0

{-
example = let x = Var "x"
          in App (Lam "x" ( x `Plus` x))  (ValInt 7)
example' = run example

example2 = let x = Var "x"; y = Var "y"
           in ((Lam "x" (Lam "y" ( x `Plus` y))) `App` (ValInt 7)) `App` (ValInt 4)
example2' = run example2

example3 = let x = Var "x"; y = Var "y"
           in Lam "x" (Lam "y" ( x `Plus` y))
example3' = run example3

example4 = let x = Var "x"; y = Var "y"
           in (Cons $ Lam "x" (Lam "y" ( x `Plus` y))) $  (Cons ( Lam "x" (Lam "y" ( x `Minus` y))) $ Nil)
example4' = run example4
-}


ex0 = showPretty  (Mult (Var "a") (Or (Var "b") (Var "c"))) 0

ex1 = showPretty (Minus (Var "y") (Minus (App (ValBool True) (ValInt (-3))) (Mult (ValBool False) (ValBool False)))) 0

ex2 = showPretty (Cons (Var "z") (Not (Not (Plus (Mult (ValInt (-18)) Nil) (Not (ValInt 2)))))) 0

ex3 = "not not (-18)"

ex4 = "(1 - false >= (if (-8) then true else true) && [] / (-7) < false * []) > (let z = (true false) == (-5) / (-6) in (false && 7) + ((-9) : (-2)))"

t1 = "5"

t2 = "thisIsAnIdentifier"

t3 = "true"

t4 = "false"

t5 = "[]"

t6 = "[   ]"

t7 = "not false"

t8 = "!5"

t9 = "not not not []"

t10 = "4 * 2"

t11 = "2 / 1"

t12 = "3 * 9 / value  * 4"

t13 = "!4 * true / not not []" --partial parse

t14 = "2 + 4"

t15 = "9 - 2"

t16 = "2 * x - 2 / 1"

t17 = "not 6 * [] - true"

t18 = "true && b "

t19 = "false || true"

t20 = "false && true || false && false"

t21 = "not false && not boolIdent || not true"

t22 = "4 && ![] || false"

t23 = " 4 : []"

t24 = "true : false : x : []" --partial parse

t25 = "4 + 2 : 5 / 1 : not false && true || z : []" --partial parse

t26 = "f 5"

t27 = "f g h 3"

t28 = " f g 8 * 2"

t29 = "f 3 : []"

t30 = "if true then 2 else 6"

t31 = "if true && false then 2 + 5 else 9 : []"

t32 = "if true then 3 else if false then 9 else 1"

t33 = "let x = 5 in x"

t34 = "let x = 5 * 7 in x : []"

t35 = "let x = true : [] in let y = 5 in y : x"

t36 = "let y = if 3 then 2 else true in let y = if true then 5 else [] in let x = 4 in x"

t37 = "\\x -> 5"

t38 = "\\x -> \\y -> x"

t39 = "\\x -> let y = x in x && y"

t40 = "\\x -> if x then true else y : []"

t41 = "\\x -> x y"

t42 = "\\x -> x : y"

t43 = "if \\x -> x then \\y -> y else \\z -> z"

t44 = "let f = \\x -> x y in \\z -> f z x"

t45 = "\\x -> x + 4 : []"

t46 = "4 * (2 + 9)"

t47 = "f (\\x -> x y) ( (4 : 3) x ) "

t48 = "f (g x) (h 4)"

t49 = "3 + if true then 5 else 9"

t50 = "if true then 5 else 9 + 3"

t51 = "(if true then 5 else 9) + 3"

t52 = "2 : \\x -> x : 1"

t53 = "8 - let x = 2 in 1 - 3"

-- These are the "weird" examples from the distribution file

t54 = "a * (b || c)"

t55 = "y - ((true (-3)) - false * false)"

t56 = "z :  not  not ((-18) * [] +  not 2)" --partial parse

t57 =  "not not (-18)" --no parse


arbitrarySizedIf ::  Int -> Gen Ast
arbitrarySizedIf m = do x <- arbitrarySizedAst (m `div` 3)
                        y <- arbitrarySizedAst (m `div` 3)
                        z <- arbitrarySizedAst (m `div` 3)
                        return $ If x y z

example1 = (Not (Not (Not (Nil))))
example1' = "not not not []"

example2 = (IntDiv (Mod (Not (ValInt 4)) (ValBool True)) (Not (Not Nil)))
example2' = "!4 * true / not not []"

example3 = (If (Lam "x" (Var "x")) (Lam "y" (Var "y")) (Lam "z" (Var "z")))
example3' = "if \\x -> x then \\y -> y else \\z -> z"

example4 = (Cons (ValChar 'a') (Cons (ValString "b") (Cons (ValInt 3) (Cons (Cons (ValBool True) Nil) (Cons (ValBool False) (Cons (Var "meep") Nil))))))
example4' = "['a', \"b\", 3, [true], [false, meep]]"

example5 = (GreaterThan (And (GreaterThanOrEquals (Minus (ValInt 1) (ValBool False)) (If (NegExp (ValInt 8)) (ValBool True) (ValBool True))) (LessThan (IntDiv Nil (NegExp (ValInt 7))) (Mult (ValBool False) Nil))) (Let "z" (Equals (App (ValBool True) (ValBool False)) (IntDiv (NegExp (ValInt 5)) (NegExp (ValInt 6)))) (Plus (And (ValBool False) (ValInt 7)) (Cons (NegExp (ValInt 9)) (Cons (NegExp (ValInt 2)) Nil)))))
example5' = "(1 - false >= (if (-8) then true else true) && [] / (-7) < false * []) > (let z = (true false) == (-5) / (-6) in (false && 7) + ((-9) : (-2)))"

example6 = (Print (Separator (IntExp (ValInt 4) (ValInt 3)) (Lam "x" (FloatDiv (FloatExp (Var "x") (ValFloat 3.0)) (ValFloat 4.0)))))
example6' = "print(4 ** 3; \\x -> x ^ 3.0 / 4.0)"

example7 = (Let "x" (ValInt 5) (If (Equals (ValChar 'a') (ValChar 'b')) (Print (Var "x")) (Mod (ValFloat 1.0) (ValFloat 0.0))))
example7' = "let x = 5 in if ('a' == 'b') then print(x) else 1.0/0.0"

example8 = (Lam "x" (If (Var "x") (ValBool True) (Cons (Var "y") Nil)))
example8' = "\\x -> if x then true else y : []"

example9 = (Separator (Print (NegExp (ValInt 5))) (Let "x" (NegExp (ValInt 10)) (Separator (Mult (Var "x") (ValInt 4)) (ValString "meep"))))
example9' = "print(-5); let x = (-10) in x * 4; \"meep\""

example10 = Print (App (App (Var "map") (Lam "x" (Plus (Var "x") (ValInt 10)))) (Cons (ValInt 1) (Cons (ValInt 2) (Cons (ValInt 3) (Cons (ValInt 4) (Cons (ValInt 5) Nil))))))
example10' = "print(map (\\x -> x+10) (1:2:3:4:5))"

composeEx = "let f = (\\x -> x + 4) in let g = (\\x -> x // 2) in f . g"

tests = testGroup "parser Test"
      [
        testCase "showPretty tests: " $
          do
            assertEqual example1' (Just (example1, "")) (parse parser (showPretty example1 0))
            assertEqual example2' (Just (example2, "")) (parse parser (showPretty example2 0))
            assertEqual example3' (Just (example3, "")) (parse parser (showPretty example3 0))
            assertEqual example4' (Just (example4, "")) (parse parser (showPretty example4 0))
            assertEqual example5' (Just (example5, "")) (parse parser (showPretty example5 0))
            assertEqual example6' (Just (example6, "")) (parse parser (showPretty example6 0))
            assertEqual example7' (Just (example7, "")) (parse parser (showPretty example7 0))
            assertEqual example8' (Just (example8, "")) (parse parser (showPretty example8 0))
            assertEqual example9' (Just (example9, "")) (parse parser (showPretty example9 0))
            assertEqual example10' (Just (example10, "")) (parse parser (showPretty example10 0)),

        testCase "showFullyParen tests: " $
          do
            assertEqual example1' (Just (example1, "")) (parse parser (showFullyParen example1))
            assertEqual example2' (Just (example2, "")) (parse parser (showFullyParen example2))
            assertEqual example3' (Just (example3, "")) (parse parser (showFullyParen example3))
            assertEqual example4' (Just (example4, "")) (parse parser (showFullyParen example4))
            assertEqual example5' (Just (example5, "")) (parse parser (showFullyParen example5))
            assertEqual example6' (Just (example6, "")) (parse parser (showFullyParen example6))
            assertEqual example7' (Just (example7, "")) (parse parser (showFullyParen example7))
            assertEqual example8' (Just (example8, "")) (parse parser (showFullyParen example8))
            assertEqual example9' (Just (example9, "")) (parse parser (showFullyParen example9))
            assertEqual example10' (Just (example10, "")) (parse parser (showFullyParen example10))


      ]
