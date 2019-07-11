{-|
Module: Exec
Description: Holds the exec function, as well as parseError and runtimeError catching mechanisms.
-}

module Exec where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import Eval
import Parser
import Check
import ParserMonad
import EnvUnsafeLog


-- | LangOut holds ParseError, RuntimeError, or Safe values, with the print and WarningMessage buffers
data LangOut =
    ParseError -- ^ retuned when the string could not be parsed
  | RuntimeError String [String] [WarningMsg]
  -- ^ retuned when there is a runtime error
  -- first String is the error message
  -- this list of Strings is what is printed before the error was encountered
  | Ret Val [String] [WarningMsg]
  -- ^ retuned when the program runs successfully and return a value
  -- The Val is the evaluation result of the program
  -- The list of String is what gets printed while running the program
  deriving (Show,Eq)
-- | surroundList takes a string that looks like list (i.e. [1,2,3]) and puts a parentheses around it
surroundList :: String -> String
surroundList "" = ""
surroundList (x:xs)
  | x == '[' = "(" ++ x:(surroundList xs)
  | x == ']' =  x:")" ++ (surroundList xs)
  | otherwise = x:surroundList xs

-- | execute the program as a string and get the result
exec :: String -> LangOut
exec s = let s' = (surroundList s); s'' = (parse parser s')
         in case s'' of
              Nothing -> ParseError
              Just (ast, h:s) -> ParseError
              Just (ast, "") -> let warnings = Set.toList (warn ast)
                                    value = run ast
                                in case value of
                                     (Ok val, buffer) -> Ret val buffer warnings
                                     (Error msg, buffer) -> RuntimeError msg buffer warnings

-- | perform static checking on the program string, may be empty if there is a parse error
warn :: Ast -> (Set WarningMsg)
warn s = check s
