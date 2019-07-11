-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework

module LangMonad where

import Control.Monad(ap)

--This monad should form the plumbing for the evaluation function
-- This is a very rough outline



-- feel free to rename LangMonad, add parameters, anything you want!
data LangMonad a -- ...

-- function that just runs the contents of LangMonad
runLangMonad = undefined

instance Functor (LangMonad) where
  fmap f ma = undefined

  
--ignore this for now
instance Applicative (LangMonad) where
  pure = return
  (<*>) = ap
  
instance Monad (LangMonad) where
  return a = undefined
  
  ma >>= f = undefined


-- You could put some monad helper functions here