{-|
Module: EnvUnsafeLog
Description: Monadic plumbing for Eval.hs
-}
module EnvUnsafeLog where

import Control.Monad(ap)
--This monad will form the plumbing for the evaluation function

-- | Unsafe holds error strings, and non-error 'Ok' values.
data Unsafe a = Error String | Ok a deriving (Show, Eq)

-- | EnvUnsafeLog holds envType, the logType and the resType
data EnvUnsafeLog envType logType resType = EnvUnsafeLog (envType -> (Unsafe resType, [logType]))

-- | function that just runs the function contained in EnvUnsafe
runEnvUnsafe ::  (EnvUnsafeLog e l a) -> e -> (Unsafe a, [l])
runEnvUnsafe (EnvUnsafeLog eu) e = eu e

-- | a way to easily return an error (for instance in do notation)
err :: String -> EnvUnsafeLog e l a
err s = EnvUnsafeLog $ \ _ -> (Error s, [])


-- | a way to easily get the entire environment (for instance in do notation)
getEnv :: EnvUnsafeLog e l e
getEnv = EnvUnsafeLog $ \ env -> (Ok env, [])

-- | places a string in the buffer value of EnvUnsafeLog
printBuffer :: String -> EnvUnsafeLog e String ()
printBuffer s = EnvUnsafeLog $ \ _ -> (Ok (), [s])
-- | adds string value to buffer value of EnvUnsafeLog
addToBuffer :: [String] -> EnvUnsafeLog e String ()
addToBuffer ss = EnvUnsafeLog $ \ _ -> (Ok (), ss)

instance Functor (EnvUnsafeLog e l) where
  -- | fmap :: (a -> b) -> EnvUnsafe a -> EnvUnsafe b
  fmap f origEnv = EnvUnsafeLog $ (\ env ->
    case (runEnvUnsafe origEnv env) of (Ok v, lst) -> (Ok $ f v, lst)
                                       (Error msg, lst) -> (Error msg, lst))
  -- make sure your implementation follows the functor laws

--ignore this for now
instance Applicative (EnvUnsafeLog e a) where
  pure = return
  (<*>) = ap

instance Monad (EnvUnsafeLog e l) where
  -- | return :: a -> EnvUnsafe a
  return val = EnvUnsafeLog $ \ env -> (Ok val, [])

  -- |(>>=) :: EnvUnsafe a -> (a -> EnvUnsafe b) -> EnvUnsafe b
  origEnv >>= f = EnvUnsafeLog $ (\ env ->
    case (runEnvUnsafe origEnv env) of
      (Ok val, lst) ->
        case (runEnvUnsafe (f val)  env) of
          (Ok val', lst') -> (Ok val', lst ++ lst')
          (Error msg', lst') -> (Error msg', lst ++ lst')
      (Error msg, lst) -> (Error msg, lst))

  -- make sure your implementation follows the Monad laws

-- technical note: this could be done more succinctly with monad transformers, but it is also good practice to do it by hand
