{-|
Module      : Ast
Description : Syntax Tree

-}
module Ast where
import Data.Map (Map)
import Data.Set (Set)
import Data.List (isSubsequenceOf)
import Data.Char
import Data.Int
import qualified Data.Map as Map
import qualified Data.Set as Set --used for lambda stuff

import HelpShow

import EnvUnsafeLog

-- | The syntax tree
data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValChar Char
         | ValInt Integer
         | ValFloat Float
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast
         | IntDiv Ast Ast
         | FloatDiv Ast Ast
         | Mod Ast Ast
         | IntExp Ast Ast
         | FloatExp Ast Ast
         | ValString String
         | Equals Ast Ast
         | NotEquals Ast Ast
         | LessThan Ast Ast
         | LessThanOrEquals Ast Ast
         | GreaterThan Ast Ast
         | GreaterThanOrEquals Ast Ast
         | NegExp Ast

         | Nil
         | Cons Ast Ast
         | ListIndex Ast Ast
         | ListConcat Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast
         | Compose Ast Ast

         | Separator Ast Ast
         | Print Ast
         deriving (Eq,Show)

--instance Show Ast where
--  show ast = showPretty ast 0

-- | The AST to show
showPretty :: Ast
              -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> Integer
              -- ^ The minimally parenthesized string representing the input Ast
            -> String
showPretty (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
showPretty (ValFloat f) _ =  if f < 0
                             then  "(" ++ show f ++ ")"
                             else show f
showPretty (ValBool True) _ =  "true"
showPretty (ValChar c) _ = show c
showPretty (ValString s) _ = show s
showPretty (ValBool False)  _  = "false"
showPretty Nil _ = "[]"
showPretty (Var s) _ = s

showPretty (Lam v bod) i = parenthesize 0 i $ "\\ " ++ v ++ " -> " ++ (showPretty bod 0)
showPretty (Let v a bod)  i = parenthesize 0 i $  "let " ++ v ++ " = " ++ (showPretty a 0) ++ " in " ++ (showPretty bod 0)
showPretty (If b t e) i = parenthesize 0 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 0) ++ " else " ++ (showPretty e 0)

showPretty (Separator l r) i = parenthesize 1 i $ (showPretty l 1) ++ " ; " ++ (showPretty r 1)
showPretty (App l r) i = parenthesize 1 i $ (showPretty l 1) ++ " " ++ (showPretty r 1)
showPretty (Compose l r) i = parenthesize 2 i $ (showPretty l 2) ++ " . " ++ (showPretty r 2)
showPretty (Cons l r) i = parenthesize 4 i $ (showPretty l 5) ++ " : " ++ (showPretty r 4)
showPretty (Or l r) i = parenthesize 6 i $ (showPretty l 6) ++ " || " ++ (showPretty r 7)
showPretty (And l r) i = parenthesize 8 i $ (showPretty l 8) ++ " && " ++ (showPretty r 9)

showPretty (Equals l r) i = parenthesize 10 i $ (showPretty l 10) ++ " == " ++ (showPretty r 11)
showPretty (NotEquals l r) i = parenthesize 10 i $ (showPretty l 10) ++ " /= " ++ (showPretty r 11)
showPretty (LessThanOrEquals l r) i = parenthesize 10 i $ (showPretty l 10) ++ " <= " ++ (showPretty r 11)
showPretty (LessThan l r) i = parenthesize 10 i $ (showPretty l 10) ++ " < " ++ (showPretty r 11)
showPretty (GreaterThanOrEquals l r) i = parenthesize 10 i $ (showPretty l 10) ++ " >= " ++ (showPretty r 11)
showPretty (GreaterThan l r) i = parenthesize 10 i $ (showPretty l 10) ++ " > " ++ (showPretty r 11)

showPretty (Minus l r) i = parenthesize 11 i $ (showPretty l 11) ++ " - " ++ (showPretty r 12)
showPretty (Plus l r) i = parenthesize 11 i $ (showPretty l 11) ++ " + " ++ (showPretty r 12)
showPretty (Mult l r) i = parenthesize 13 i $ (showPretty l 13) ++ " * " ++ (showPretty r 14)
showPretty (IntDiv l r) i = parenthesize 13 i $ (showPretty l 13) ++ " // " ++ (showPretty r 14)
showPretty (FloatDiv l r) i = parenthesize 13 i $ (showPretty l 13) ++ " / " ++ (showPretty r 14)
showPretty (Mod l r) i = parenthesize 13 i $ (showPretty l 13) ++ " % " ++ (showPretty r 14)
showPretty (IntExp b e) i = parenthesize 14 i $ (showPretty b 14) ++ " ** " ++ (showPretty e 15)
showPretty (FloatExp b e) i = parenthesize 14 i $ (showPretty b 14) ++ " ^ " ++ (showPretty e 15)
showPretty (ListIndex lst idx) i = parenthesize 15 i $ (showPretty lst 15) ++ " !! " ++ (showPretty idx 16)
showPretty (ListConcat lst1 lst2) i = parenthesize 15 i $ (showPretty lst1 15) ++ " ++ " ++ (showPretty lst2 16)
showPretty (NegExp x) i = parenthesize 15 i $ " - " ++ (showPretty x 15)
showPretty (Not l ) i = parenthesize 15 i $  " not " ++ (showPretty l 15)
showPretty (Print x) i = parenthesize 15 i $ " print( " ++ (showPretty x 15) ++ " )"
--
--

-- | output the fully parenthesized statement
showFullyParen :: Ast
                   -- ^ The Ast to show
                -> String
                   -- ^ the fully parenthesized string representing the input Ast
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValFloat f) = "(" ++ show f ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (ValChar c) = "(" ++ show c ++ ")"
showFullyParen (ValString s) = "(" ++ show s ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (NegExp x) = "(-" ++ (showFullyParen x) ++ ")"
showFullyParen (Not a) = "(" ++ " not " ++ (showFullyParen a) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (IntDiv l r) = "(" ++ (showFullyParen l) ++ " // " ++ (showFullyParen r) ++ ")"
showFullyParen (FloatDiv l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (Mod l r) = "(" ++ (showFullyParen l) ++ " % " ++ (showFullyParen r) ++ ")"
showFullyParen (IntExp b e) = "(" ++ (showFullyParen b) ++ " ** " ++ (showFullyParen e) ++ ")"
showFullyParen (FloatExp b e) = "(" ++ (showFullyParen b) ++ " ^ " ++ (showFullyParen e) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen Nil = "( [] )"
showFullyParen (ListIndex lst idx) = "(" ++ (showFullyParen lst) ++ " !! " ++ (showFullyParen idx) ++ ")"
showFullyParen (ListConcat lst1 lst2) = "(" ++ (showFullyParen lst1) ++ " ++ " ++ (showFullyParen lst2) ++ ")"
showFullyParen (Equals l r) = "(" ++ (showFullyParen l) ++ " == " ++ (showFullyParen r) ++ ")"
showFullyParen (NotEquals l r) = "(" ++ (showFullyParen l) ++ " /= " ++ (showFullyParen r) ++ ")"
showFullyParen (LessThan l r) = "(" ++ (showFullyParen l) ++ " < " ++ (showFullyParen r) ++ ")"
showFullyParen (LessThanOrEquals l r) = "(" ++ (showFullyParen l) ++ " <= " ++ (showFullyParen r) ++ ")"
showFullyParen (GreaterThan l r) = "(" ++ (showFullyParen l) ++ " > " ++ (showFullyParen r) ++ ")"
showFullyParen (GreaterThanOrEquals l r) = "(" ++ (showFullyParen l) ++ " >= " ++ (showFullyParen r) ++ ")"
showFullyParen (Separator l r) = "(" ++ (showFullyParen l) ++ " ; " ++ (showFullyParen r) ++ ")"
showFullyParen (Print x) = "print(" ++ (showFullyParen x) ++ ")"
showFullyParen (Compose f g) = "(" ++ (showFullyParen f) ++ " . " ++ (showFullyParen g) ++ ")"
