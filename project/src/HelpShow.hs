
{- | 
Module: HelpShow
Description: This is a helper module for the functions showPretty and showFullyParen
-}
module HelpShow where

import Control.Monad(ap)

import Data.Set (Set)
import qualified Data.Set as Set



-- | Finds a new name that doesn't conflict with a set of other names
findName :: String -> Set String -> String
findName str avoidNames | Set.member str avoidNames = findName (str ++ "'") avoidNames
                        | otherwise                 = str


-- | Generates an infinite list of unique variable names
varNames = [ [v] | v <- ['a' .. 'z']] ++ fmap (\ v -> v ++ "'") varNames

-- | Datatype for new variable names
data NewNames a = NewNames (Set String -> Integer -> (a,Integer)) -- TODO: move to lib

-- | Function to get the value contained in NewNames
runNewNames (NewNames f ) = f

-- | Magic function that gets the first something in NewNames' something
execNewNames (NewNames f ) a = fst $ f a 0

instance Functor NewNames where
  -- fmap :: (a -> b) -> NewNames a -> NewNames b
  fmap f (NewNames g) =  NewNames $ \ avoid i -> case g avoid i of (a, i') -> (f a, i')

--ignore this for now
instance Applicative NewNames where
  pure = return
  (<*>) = ap


instance Monad NewNames where
  --return :: a -> Parser a
  return a =   NewNames $ \ avoid i -> (a, i)


  --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (NewNames g) >>= f =  NewNames $ \ avoid i -> case g avoid i of (a, i') -> runNewNames (f a) avoid i'

  
-- | The entire point of this monad--generates a new name
freshName :: NewNames String
freshName = NewNames $ \ avoid i -> (filter (\v -> not $ Set.member v avoid) varNames !! (fromIntegral i), i + 1)





-- | helper function to 'showPretty'
-- It determines whether to parenthesize current expression 
-- based on the precedence level of current operator and outer operator.
-- The smaller precedence level means the operator have higher precedence.
-- If the inner expression (current expression) has lower precedence (larger precedence level),
-- then the inner expression (current expression) will need to be parenthesized.
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

