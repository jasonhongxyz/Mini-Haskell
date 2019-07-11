module showPrettyHints where

-- This is a version of the language for the week10 homework
-- with all the evaluation cut out, just to explore the
-- way that showPretty works. 

-- Here is the abstract syntax tree for our language

data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Integer
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast

         | Eq Ast Ast | NEq Ast Ast

         | Nil
         | Cons Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast
--           deriving (Eq,Show) -- helpful to use this during testing
         deriving Eq


instance Show Ast where
  show ast = showPretty ast 0




-- This is helpful for testing and debugging
showFullyParen :: Ast -> String
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Eq l r) = "(" ++ (showFullyParen l) ++ " == " ++ (showFullyParen r) ++ ")"
showFullyParen (NEq l r) = "(" ++ (showFullyParen l) ++ " /= " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (Div l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen Nil = "( [] )"


-- provide a nice show with minimal parentheses, for testing and documentation



-- Here is the way to set up showPretty. There are three important numbers:

-- showPretty (Plus l r) i    = parenthesize 10 i $ (showPretty l 10) ++ " + "  ++ (showPretty r 11)
                                             --                   --                             --
--                                           k                    n                              m
  
-- The first number, k, is essentially the precedence of the operator (e.g., 12 for Plus), although
-- these are relative (exact values don't matter, just the relative ordering), and
-- ALSO you should give Let, Lam, and If (the mix-fix operators) the numbers k = n = m = 1. 

-- Thereafter give numbers in increasing order of precedence. Same
-- precedence should have the same number.

-- To deal with associativity, you should have

--       n      n        m
--      ---    ---      ---

--       k      k       (k+1)    for L associative  (e.g., Plus)

--       k      (k+1)   k        for R associative  (e.g., Cons)

--       k      (k+1)   (k+1)    for non-associative (relational operators)

--       k      k                for prefix (not, unary minus)

-- Look through the definitions for Plus, Cons, Eq, and Not to see how this
-- is done. You can extend this easily to your grammar.

-- IMPORTANT NOTE: In our project, since the minus sign is overloaded in two different precedence levels,
-- this is an ambiguity if you pretty-print -5 as "-5", since (f - 5) can be either
-- a function f applied to (-5) or a variable f minus 5.
-- To avoid this, always pretty print a unary minus expression with parentheses around it.
-- Show, showPretty (UnaryMinus (ValInt 5)) => "( - 5 )


showPretty :: Ast -> Integer -> String
showPretty (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty Nil _ = "[]"
showPretty (Var s) _ = s
showPretty (Lam v bod) i   = parenthesize 1 i  $ "\\ " ++ v ++ " -> "        ++ (showPretty bod 100)
showPretty (Let v a bod) i = parenthesize 1 i  $  "let " ++ v ++ " = "       ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i    = parenthesize 1 i  $  "if " ++ (showPretty b 1)  ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)
showPretty (App l r) i     = parenthesize 2 i  $ (showPretty l 2)  ++ " "    ++ (showPretty r 3)
showPretty (Cons l r) i    = parenthesize 4 i  $ (showPretty l 5)  ++ " : "  ++ (showPretty r 4)
showPretty (Or l r) i      = parenthesize 6 i  $ (showPretty l 6)  ++ " || " ++ (showPretty r 7)
showPretty (And l r) i     = parenthesize 8 i  $ (showPretty l 8)  ++ " && " ++ (showPretty r 9)
showPretty (Eq l r) i      = parenthesize 10 i  $ (showPretty l 11) ++ " && " ++ (showPretty r 11)
showPretty (NEq l r) i     = parenthesize 10 i  $ (showPretty l 11) ++ " && " ++ (showPretty r 11)
showPretty (Minus l r) i   = parenthesize 12 i $ (showPretty l 12) ++ " - "  ++ (showPretty r 13)
showPretty (Plus l r) i    = parenthesize 12 i $ (showPretty l 12) ++ " + "  ++ (showPretty r 13)
showPretty (Mult l r) i    = parenthesize 14 i $ (showPretty l 14) ++ " * "  ++ (showPretty r 15)
showPretty (Div l r) i     = parenthesize 14 i $ (showPretty l 14) ++ " / "  ++ (showPretty r 15)
showPretty (Not l ) i      = parenthesize 16 i $  " ! " ++ (showPretty l 16)





-- | helper function to 'showPretty'
-- It determines whether to parenthesize current expression 
-- based on the precedence level of current operator and outer operator.
--
-- Example: 
-- @
--    Mult 
--      (Plus (LiteralInt 1) (Literal Int 2))
--      LiteralInt 2
-- @
-- The Plus inside need to be parenthesized, 
-- Because the inner expression (current expression) Plus 
-- has lower precedence than the outer expression Mult.

parenthesize :: Integer -- ^ the precedence level of outer expression
              -> Integer -- ^ the precedence level of the current expression
              -> String -- ^ string representation current expression
              -> String -- ^ the properly (not necessarily fully) parenthesized current expression

parenthesize outerLevel curLevel showExp 
  | outerLevel < curLevel = "(" ++ showExp ++ ")"
  | otherwise             =        showExp
