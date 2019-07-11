{-|
Module      : GeneratorExample
Description : This is example of quickCheck generator

This is a example of quickCheck generator for the type
'Term' (the lambda term in lamdba calculus)
This example make use of the helper function
'genSizedBinaryExp'
-}
{-# LANGUAGE DeriveGeneric #-}
-- You need this extension to use genericShrink
-- without genericShrink, the shrinker will be nearly impossible to implement.

module GeneratorExampleWithHelper where

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


  -- | This create a generator for a given binary expression
  -- Will generate a Term which is the root operator is 
  -- the input binary expression
  -- This helper function is silly here,
  -- but will be very useful in your project
  genSizedBinaryExp :: (Term -> Term -> Term)  -- ^ the binary expression you want to create the generator
                      -> Int  -- ^ input size
                      -> Gen Term  -- ^ return the generator for the binary expression
  genSizedBinaryExp op size =
    -- you need to figure out the implementation for this function
    -- 'genSizedTrinaryExp' will be helpful
    undefined  


  -- | This create a generator for a given trinary expression
  -- Will generate a Term which is the root operator is 
  -- the input trinary expression
  -- This helper function is silly here,
  -- but will be very useful in your project
  genSizedTrinaryExp :: (Term -> Term -> Term -> Term)  -- ^ the trinary expression you want to create the generator
                      -> Int  -- ^ input size
                      -> Gen Term  -- ^ return the generator for the binary expression
  genSizedTrinaryExp op size =
    do 
      -- generate three input for op
      -- each of them is 1/3 of the input size
      -- to garentee the generated term has 
      -- the size 'size'
      inp1 <- genSizedTerm $ size `div` 3
      inp2 <- genSizedTerm $ size `div` 3
      inp3 <- genSizedTerm $ size `div` 3
      -- return the final term
      return $ op inp1 inp2 inp3
  
  -- | generate a var
  genVar :: Gen Term
  genVar = 
    do 
      varName <- arbitrary
      return $ Var varName
  

  -- | generate a sized application
  genSizedApp :: Int -> Gen Term
  genSizedApp = genSizedBinaryExp App

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
      body <- genSizedTerm $ size - 1 
      return $ Lam paraName body
      
      
  -- | generate a random test show Lambda term
  genSizedTerm ::  Int -> Gen Term
  genSizedTerm size 
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
    arbitrary = sized genSizedTerm
    -- | use generic shrink to 
    -- automatically implement shrinker based on 
    -- subexpression
    shrink = genericShrink 
