{- |
Module: Eval
Description: Eval is the evaluation system, controls what value will be returned
-}

module Eval where

import Data.Map (Map)
import Data.Set (Set)
import Data.List (isSubsequenceOf)
import Data.Char
import Data.Int
import qualified Data.Map as Map
import qualified Data.Set as Set

import HelpShow
import Ast
import EnvUnsafeLog

-- | the goal of the program is to return a value
data Val = I Integer | B Bool | F Float | C Char
         | Ls [Val] | S [Char]
         | Fun (Val -> (Unsafe Val, [String])) -- since this is a functional language, one thing that can be returned is a function
         | Err String

instance Show Val where
  show (I i) = show i
  show (F f) = show f
  show (B b) = show b
  show (C c) = show c
  show (S s) = show s
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function
  show (Err msg) = "Error: " ++ msg

instance Eq Val where
  (I x) == (I y) = x == y
  (F x) == (F y) = x == y
  (B x) == (B y) = x == y
  (C x) == (C y) = x == y
  (S x) == (S y) = x == y
  (Ls []) == (Ls []) = True
  (Ls (x:xs)) == (Ls (y:ys)) = (x == y) && ((Ls xs) == (Ls ys))
  _ == _ = False

-- | Env is the environment mapping a string to a Val
type Env = Map String Val

-- | Library of useful functions for casting, lists, etc.
stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> ((Ok $ Ls ls), [])
                                   _         -> (Error $ "Argument (" ++ (show v) ++ ") must be a non empty list", [])),
   ("head", Fun $ \ v -> case v of Ls (head:_) -> ((Ok $ head), [])
                                   _           -> (Error "Can only call head on a non empty list", [])),
   ("len", Fun $ \ v -> case v of Ls ls -> ((Ok $ (I (fromIntegral (length ls)))), [])
                                  _     -> (Error $ "Argument (" ++ (show v) ++ ") must be a non empty list", [])),
   ("ord", Fun $ \ v -> case v of C ch -> ((Ok $ (I (fromIntegral (ord ch)))), [])
                                  _    -> (Error "Can only call ord on a char", [])),
   ("chr", Fun $ \ v -> case v of I integer -> ((Ok $ (C (chr (fromIntegral integer)))), [])
                                  _    -> (Error "Can only call chr on a char", [])),
   ("float", Fun $ \ v -> case v of I integer -> ((Ok $ (F (realToFrac (fromIntegral integer)))), [])
                                    _    -> (Error "Can only call float on an int", [])),
   ("int", Fun $ \ v -> case v of F fl -> ((Ok $ (F (fromIntegral (truncate fl)))), [])
                                  _    -> (Error "Can only call int on a float", [])),
   ("elem", Fun $ \ var -> ((Ok $ Fun $ \ list -> case list of
                                             Ls ls -> ((Ok $ (B (elem var ls))), [])
                                             _     -> (Error "Can only call elem on a list", [])), [])),
   ("filter", Fun $ \ func -> ((Ok $ Fun $ \ list -> case list of
                                             Ls ls -> case func of
                                               Fun fn -> ((Ok $ (Ls (filterHelper fn ls))), [])
                                               _ -> (Error "First argument of filter must be a function", [])
                                             _     -> (Error "Can only call filter on a list", [])), [])),
   ("map", Fun $ \ func -> ((Ok $ Fun $ \ list -> case list of
                                             Ls ls -> case func of
                                               Fun fn -> ((Ok $ Ls (mapHelper fn ls)), [])
                                               _ -> (Error "First argument of map must be a function", [])
                                             _     -> (Error "Can only call map on a list", []), [])))]
-- | to be used for EnvUnsafeLog in eval
local :: (r -> r) -> EnvUnsafeLog r String a -> EnvUnsafeLog r String a
local changeEnv comp  = EnvUnsafeLog (\e -> runEnvUnsafe comp (changeEnv e) )

-- | helper method for persisting the print logs of applied or composed functions
appendToBuffer :: (Unsafe Val, [String]) -> EnvUnsafeLog Env String Val
appendToBuffer (Ok val, lst) = do addToBuffer lst
                                  return val
appendToBuffer (Error msg, lst) = do addToBuffer lst
                                     err msg
-- | filter for stdLib, helper functions to get items to be passed through
filterHelper :: (a -> (Unsafe Val, [b])) -> [a] -> [a]
filterHelper _ [] = []
filterHelper func (head:body) = case (func head) of
  (Ok (B True), log) -> [head] ++ (filterHelper func body)
  (Ok (B False), log) -> (filterHelper func body)
  _ -> (head:body)

-- | map for stdLib, helper function to get items to be passed through. Version of map.
mapHelper :: (Val -> (Unsafe Val, [b])) -> [Val] -> [Val]
mapHelper _ [] = []
mapHelper func (head:body) = let (res, log) = func head in case res of
                                                             (Ok val) -> val:(mapHelper func body)
                                                             (Error msg) -> head:body
-- | determines if the index given is valid, will catch if too large
validListIndex :: [Val] -> Integer -> Either String Val
validListIndex lst idx
  | (fromIntegral idx) >= (length lst) =
    Left $ "Index too large. Index given: " ++ (show idx) ++ " but maximum is: " ++ (show ((length lst) - 1))
  | otherwise          = Right (lst !! (fromIntegral idx))

-- | helper function for eval Var that returns the EnvUnsafeLog of a Var
valOf :: String -> EnvUnsafeLog Env String Val
valOf var = do env <- getEnv
               case (Map.lookup var env) of
                  Just i  -> return i
                  Nothing -> err $ "Variable " ++ var ++ " is not defined or is not in scope"


-- | helper functions that take care of type issues (use a "Error" when things have the wrong type
evalNum :: Ast -> EnvUnsafeLog Env String (Either Float Integer)
evalNum a = do a' <- eval a
               case a' of
                 F f -> return (Left f)
                 I i -> return (Right i)
                 _   -> err $ (showPretty a 0) ++ " is not a number"
-- | helper function for eval Int. Will determine if a Val is an integer, catches and errors otherwise
evalInt :: Ast -> EnvUnsafeLog Env String Integer
evalInt a = do a' <- eval a
               case a' of
                 I i -> return i
                 _ -> err $ (showPretty a 0) ++ " is not an int"
-- | helper function for eval Float. Will determine if a Val is a float, catches and errors otherwise
evalFloat :: Ast -> EnvUnsafeLog Env String Float
evalFloat a = do a' <- eval a
                 case a' of
                   F f -> return f
                   _ -> err $ (showPretty a 0) ++ " is not a float"
-- | helper function for eval char. Will determine if a Val is a Char, catches and errors otherwise
evalChar :: Ast -> EnvUnsafeLog Env String Char
evalChar a = do a' <- eval a
                case a' of
                  C c -> return c
                  _ -> err $ (showPretty a 0) ++ " is not a char"
-- | helper function for eval bool. Will determine if a Val is a Bool, catches and errors otherwise
evalBool :: Ast -> EnvUnsafeLog Env String Bool
evalBool a = do a' <- eval a
                case a' of
                  B b -> return b
                  _ -> err $ (showPretty a 0) ++ " is not a bool"
-- | helper function for eval list. Will determine if a Val is a List, catches and errors otherwise
evalList :: Ast -> EnvUnsafeLog Env String [Val]
evalList a = do a' <- eval a
                case a' of
                  Ls x -> return x
                  _ -> err $ (showPretty a 0) ++ " is not a list"
-- | helper function for functions. Will determine if a Val is a function, catches and errors otherwise.
evalFun :: Ast -> EnvUnsafeLog Env String (Val -> (Unsafe Val, [String]))
evalFun a = do a' <- eval a
               case a' of
                 Fun a' -> return a'
                 other -> err $ (show other) ++ " is not a function"
-- | helper function for eval Print. Will add the string to the printBuffer, and return the EnvUnsafeLog with the buffer
evalPrint :: Ast -> EnvUnsafeLog Env String Val
evalPrint a = do a' <- eval a
                 let str = show a'
                 printBuffer str
                 return a'

-- | helper function for eval Equals. Allows for multiple Val types to be considered "equal"
equals :: Ast -> Ast -> EnvUnsafeLog Env String Val
equals x y =
  do x' <- eval x
     y' <- eval y
     return (B (x' == y'))
-- | helper function for eval NotEquals. Allows for multiple Val types to be considered "notEqual"
notEquals :: Ast -> Ast -> EnvUnsafeLog Env String Val
notEquals x y =
  do x' <- eval x
     y' <- eval y
     return (B (not $ x' == y'))
-- | helper function for eval LessThan. Allows for multiple Val types to be considered "lessThan"
lessThan :: Ast -> Ast -> EnvUnsafeLog Env String Val
lessThan x y =
  do x' <- eval x
     y' <- eval y
     case (x', y') of
       (I x'', I y'') -> return (B (x'' < y''))
       (F x'', F y'') -> return (B (x'' < y''))
       (B x'', B y'') -> return (B (x'' < y''))
       (C x'', C y'') -> return (B (x'' < y''))
       (S x'', S y'') -> return (B (x'' < y''))
       _              -> err $ "Types don't match. Can only compare values of same type."
-- | helper function for eval LessThanOrEquals. Allows for multiple Val types to be considered "lessThanOrEquals"
lessThanOrEquals :: Ast -> Ast -> EnvUnsafeLog Env String Val
lessThanOrEquals x y =
  do x' <- eval x
     y' <- eval y
     case (x', y') of
       (I x'', I y'') -> return (B (x'' <= y''))
       (F x'', F y'') -> return (B (x'' <= y''))
       (B x'', B y'') -> return (B (x'' <= y''))
       (C x'', C y'') -> return (B (x'' <= y''))
       (S x'', S y'') -> return (B (x'' <= y''))
       _              -> err $ "Types don't match. Can only compare values of same type."
-- | helper function for eval GreaterThan. Allows for multiple Val types to be considered "greaterThan"
greaterThan :: Ast -> Ast -> EnvUnsafeLog Env String Val
greaterThan x y =
  do x' <- eval x
     y' <- eval y
     case (x', y') of
       (I x'', I y'') -> return (B (x'' > y''))
       (F x'', F y'') -> return (B (x'' > y''))
       (B x'', B y'') -> return (B (x'' > y''))
       (C x'', C y'') -> return (B (x'' > y''))
       (S x'', S y'') -> return (B (x'' > y''))
       _              -> err $ "Types don't match. Can only compare values of same type."
-- | helper function for eval GreaterThanOrEquals. Allows for multiple Val types to be considered "greaterThanOrEquals"
greaterThanOrEquals :: Ast -> Ast -> EnvUnsafeLog Env String Val
greaterThanOrEquals x y =
  do x' <- eval x
     y' <- eval y
     case (x', y') of
       (I x'', I y'') -> return (B (x'' >= y''))
       (F x'', F y'') -> return (B (x'' >= y''))
       (B x'', B y'') -> return (B (x'' >= y''))
       (C x'', C y'') -> return (B (x'' >= y''))
       (S x'', S y'') -> return (B (x'' >= y''))
       _              -> err $ "Types don't match. Can only compare values of same type."
-- | Main eval function: will evaluate the inputted AST
eval :: Ast -> EnvUnsafeLog Env String Val
eval (ValBool bool) = return (B bool)
eval (ValInt int) = return (I int)
eval (ValChar char) = return (C char)
eval (ValString str) = return (S str)
eval (ValFloat flo) = return (F flo)
eval (Var str) = valOf str
eval (Equals x y) = equals x y
eval (NotEquals x y) = notEquals x y
eval (LessThan x y) = lessThan x y
eval (LessThanOrEquals x y) = lessThanOrEquals x y
eval (GreaterThan x y) = greaterThan x y
eval (GreaterThanOrEquals x y) = greaterThanOrEquals x y
eval (And x y) =
  do x' <- evalBool x
     y' <- evalBool y
     return (B (x' && y'))
eval (Or x y) =
  do x' <- evalBool x
     y' <- evalBool y
     return (B (x' || y'))
eval (NegExp x) =
  do x' <- evalNum x
     case x' of
       Left f -> return (F (0 - f))
       Right i -> return (I (0 - i))
eval (Not x) =
  do x' <- evalBool x
     return (B (not x'))
eval (Plus x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Left f1, Left f2)   -> return (F (f1 + f2))
       (Right i1, Right i2) -> return (I (i1 + i2))
       (Right _, Left _)    -> err $ "TypeMismatch: Cannot add integer " ++ (showPretty x 0) ++ " and float " ++ (showPretty y 0)
       (Left _, Right _)    -> err $ "TypeMismatch: Cannot add float " ++ (showPretty x 0) ++ " and integer " ++ (showPretty y 0)
eval (Minus x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Left f1, Left f2)   -> return (F (f1 - f2))
       (Right i1, Right i2) -> return (I (i1 - i2))
       (Right _, Left _)    -> err $ "TypeMismatch: Cannot subtract integer " ++ (showPretty x 0) ++ " and float " ++ (showPretty y 0)
       (Left _, Right _)    -> err $ "TypeMismatch: Cannot subtract float " ++ (showPretty x 0) ++ " and integer " ++ (showPretty y 0)
eval (Mult x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Left f1, Left f2)   -> return (F (f1 * f2))
       (Right i1, Right i2) -> return (I (i1 * i2))
       (Right _, Left _)    -> err $ "TypeMismatch: Cannot multiply integer " ++ (showPretty x 0) ++ " and float " ++ (showPretty y 0)
       (Left _, Right _)    -> err $ "TypeMismatch: Cannot multiply float " ++ (showPretty x 0) ++ " and integer " ++ (showPretty y 0)
eval (IntDiv x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Right i1, Right i2) -> if i2 == 0 then err "Error: Division-by-Zero" else return (I (i1 `div` i2))
       _ -> err "TypeMismatch: Can only use // with Integer types"
eval (FloatDiv x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Left f1, Left f2) -> if f2 == 0.0 then err "Error: Division-by-Zero" else return (F (f1 / f2))
       _ -> err "TypeMismatch: Can only use / with Float types"
eval (Mod x y) =
  do x' <- evalNum x
     y' <- evalNum y
     case (x', y') of
       (Right i1, Right i2) -> if i2 == 0 then err "Error: Mod-by-Zero is undefined" else return (I (i1 `mod`i2))
       _ -> err "TypeMismatch: Can only use % with Integer types"
eval (IntExp b e) =
  do b' <- evalNum b
     e' <- evalNum e
     case (b', e') of
       (Right i1, Right i2) -> return (I (i1 ^ i2))
       _ -> err "TypeMismatch: Can only use ** with Integer types"
eval (FloatExp b e) =
  do b' <- evalNum b
     e' <- evalNum e
     case (b', e') of
       (Left f1, Left f2) -> return (F (f1 ** f2))
       _ -> err "TypeMismatch: Can only use ^ with Float types"
eval (Nil) = return (Ls [])
eval (Cons x y) =
  do x' <- eval x
     y' <- eval y
     case (y') of
       Ls list -> return (Ls (x':list))
       _ -> err $ "Second term (" ++ (showPretty y 0) ++ ") must be a list"
eval (ListConcat x y) =
  do x' <- eval x
     y' <- eval y
     case (x', y') of
       (Ls x'', Ls y'') -> return (Ls (x'' ++ y''))
       (Ls x'', _) -> err $ "TypeMismatch: second argument (" ++ (showPretty y 0) ++ ") must be a list"
       (_, Ls y'') -> err $ "TypeMismatch: first argument (" ++ (showPretty x 0) ++ ") must be a list"
       (_, _) -> err $ "TypeMismatch: both arguments (" ++ (showPretty x 0) ++ "), (" ++ (showPretty y 0) ++ ") must be lists"
eval (ListIndex lst idx) =
  do lst' <- evalList lst
     idx' <- evalInt idx
     case (validListIndex lst' idx') of
       Left errorMsg -> err errorMsg
       Right val -> return val
eval (If condition ifTrue ifFalse) =
  do condition' <- eval condition
     case (condition') of
       B True -> eval ifTrue
       B False -> eval ifFalse
       _ -> err $ "Condition (" ++ (showPretty condition 0) ++ ") must evaluate to a boolean"
eval (Separator ast1 ast2) =
  do ast1' <- eval ast1
     ast2' <- eval ast2
     return ast2'
eval (Print ast) =
  do ast' <- evalPrint ast
     return ast'
eval (Let var val bod) =
  do val' <- eval val
     local (Map.insert var val') (eval bod)
eval (App x y) =
  do x' <- evalFun x
     y' <- eval y
     case (x' y') of
       (Ok val, lst) -> appendToBuffer (Ok val, lst)
       (Error msg, lst) -> appendToBuffer (Error msg, lst)
eval (Lam x bod) =
  do env <- getEnv
     return (Fun (\v -> runEnvUnsafe (eval bod) (Map.insert x v env)))
eval (Compose f g) =
  do f' <- evalFun f
     g' <- evalFun g
     return (Fun $ \x -> case (g' x) of
       (Ok val, lst) -> case (f' val) of (Ok val', lst') -> (Ok val', lst ++ lst')
                                         (Error msg, lst') -> (Error msg, lst ++ lst')
       (Error msg, ls) -> (Error msg, ls))


-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: Ast  -- ^ run this Ast
      -> (Unsafe Val, [String])  -- ^ (error message or result value, all the printings)
run a = runEnvUnsafe (eval a) stdLib

