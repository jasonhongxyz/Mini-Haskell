{-|
Module      : GeneratorExample
Description : This is example of quickCheck generator

This is a example of quickCheck generator for the type
'Term' (the lambda term in lamdba calculus)
This example did not make use of the helper function
'genSizedBinaryExp'
-}
{-# LANGUAGE DeriveGeneric #-}
-- You need this extension to use genericShrink
-- without genericShrink, the shrinker will be nearly impossible to implement.

module GeneratorExample where

  import Test.Tasty.QuickCheck
  import GHC.Generics


  -- | the type for variable name
  -- this type makes implementing
  -- Arbitrary type class a lot easier
  -- since if you use string
  -- arbitrary will generate any random string
  -- many possiblly are not valid variable name 
  -- (for example, empty string)
  data VarName = 
    -- | the string is the real variable name
    VarName String  
    deriving Eq


  instance Show VarName where 
    -- | show a variable name is just showing the string it contains
    show (VarName var) = var  


  -- | the get the string from variable name
  getVarString :: VarName -> String 
  getVarString (VarName name) = name


  instance Arbitrary VarName where 
    -- | only generating x y z as var name
    -- if there is too many possible variable name
    -- when you eval the program
    -- almost all of the program will fail with "use of undefined variable"
    arbitrary = fmap VarName $ elements ["x", "y", "z"]
    -- | do not shrink a single variable name
    -- shrinking will result in empty variable name
    shrink varName = []


  -- | same lambda term definition as week9/src/hints/CapteureAvoiding.hs                       
  data Term = 
    -- | a single variable is a term
    Var VarName
    -- | application of two terms form a term
    | App Term Term
    -- | lambda abstraction from a term
    | Lam VarName Term 
    -- generic is used for genericShrink
    deriving (Show, Eq, Generic)


  -- | generate a var
  genVar :: Gen Term
  genVar = 
    do 
      varName <- arbitrary
      return $ Var varName
  

  -- | generate a sized application
  genSizedApp :: Int -> Gen Term
  genSizedApp size =
    do 
      -- generate a sized right term
      rTerm <- genSizedShowTerm $ size `div` 2
      -- generate a sized left term
      lTerm <- genSizedShowTerm $ size `div` 2
      -- return
      return $ App rTerm lTerm
  

  -- | generate a sized Lambda
  genSizedLam :: Int -> Gen Term
  genSizedLam size =
    do 
      -- generate a variable name
      -- arbitrary is a type class method
      -- it will automatically know we are generating a varname
      -- given type inference
      paraName <- arbitrary
      -- generate the the body 
      body <- genSizedShowTerm $ size - 1 
      return $ Lam paraName body
      
      
  -- | generate a random test show Lambda term
  genSizedShowTerm ::  Int -> Gen Term
  genSizedShowTerm size 
    -- terminal case, when the size is too small
    -- genetate a terminal case (think CFG)
    | size < 1 = genVar
    | otherwise = 
      -- pick one thing to generate from the list
      oneof [
        genVar,
        genSizedApp size,
        genSizedLam size
        ]

  instance Arbitrary Term where
    -- | generate a sized show term
    arbitrary = sized genSizedShowTerm
    -- | use generic shrink to 
    -- automatically implement shrinker based on 
    -- subexpression
    shrink = genericShrink 
