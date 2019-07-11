{- |
Module: Check
Description: This module scans a line of code for undefined variables and throws a warning if it detects one
-}

module Check where

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Ast
import Eval

-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors
data WarningMsg =
    UndefinedVarUse String  -- ^ This is the Warning for use of Undefined variable name
  -- ...
  deriving (Show,Eq,Ord)

-- | perform static checking on the Ast
-- the output a set of warning on that input Ast
check :: Ast -> Set WarningMsg
check ast = check' ast (Map.keysSet stdLib) Set.empty

-- | function to check if variable is defined in scope of parent
inScopeVar :: String -> Set String -> Bool
inScopeVar var scope = Set.member var scope

-- | main helper function for check--handles all cases
check' :: Ast -> Set String -> Set WarningMsg -> Set WarningMsg
check' (ValInt _) _ warnings = warnings
check' (ValBool _) _ warnings = warnings
check' (ValFloat _) _ warnings = warnings
check' (ValString _) _ warnings = warnings
check' (ValChar _) _ warnings = warnings
check' (Var v) scope warnings =
  if (inScopeVar v scope)
     then warnings
     else Set.insert (UndefinedVarUse (v ++ " is not in scope")) warnings
check' (Lam boundVar bod) scope warnings =
  check' bod (Set.insert boundVar scope) warnings
check' (Let var val bod) scope warnings =
  (check' bod (Set.insert var scope) warnings)
  `Set.union` (check' val scope warnings)
check' (Equals x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (NotEquals x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (LessThan x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (LessThanOrEquals x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (GreaterThan x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (GreaterThanOrEquals x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (And x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (Or x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (Not x) scope warnings = check' x scope warnings
check' (NegExp x) scope warnings = check' x scope warnings
check' (Plus x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (Minus x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (Mult x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (Mod x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (IntDiv x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (FloatDiv x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (IntExp x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (FloatExp x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (If cond ifTrue ifFalse) scope warnings =
  let condCheck = check' cond scope warnings
      ifTrueCheck = check' ifTrue scope warnings
      ifFalseCheck = check' ifFalse scope warnings
  in condCheck `Set.union` ifTrueCheck `Set.union` ifFalseCheck
check' (Cons x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (Nil) _ warnings = warnings
check' (ListIndex lst idx) scope warnings =
  (check' lst scope warnings) `Set.union` (check' idx scope warnings)
check' (ListConcat lst1 lst2) scope warnings =
  (check' lst1 scope warnings) `Set.union` (check' lst2 scope warnings)
check' (App x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (Separator x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
check' (Print x) scope warnings = check' x scope warnings
check' (Compose x y) scope warnings =
  (check' x scope warnings) `Set.union` (check' y scope warnings)
