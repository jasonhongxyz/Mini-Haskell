{- |
Module: ParserMonad
Description: Monad used for parsing code
-}

module ParserMonad where
import Control.Monad(ap)

import Data.Char

-- If your interested in this style of parsing look at the parsec, megaparsec, attoparsec libraries

-- | the type of a parser
data Parser a = Parser (String -> Maybe (a, String))

-- | invoke a parser
--
-- >>> parese (literal "test") "test1"
-- Just ("test", "1")
--
-- >>> parese (literal "test") "hello"
-- Nothing
parse :: Parser a -> (String -> Maybe (a, String))
parse (Parser f) = f


instance Functor Parser where
  -- map the function f to the result of the Parser
  fmap f (Parser pa) =  Parser $ \ x -> case pa x of
                                          Nothing        -> Nothing
                                          Just (a, rest) -> Just (f a, rest)

--ignore this for now
instance Applicative Parser where
  pure = return
  (<*>) = ap


instance Monad Parser where
  -- turns a input structure into a parser
  -- by constructing a parser that
  --  * returns the input structure
  --  * do not change the string that is parsing
  return a =  Parser $ \ x -> Just (a,x)


  --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser pa) >>= f = Parser $ \ x ->  case pa x of
                                         Nothing       -> Nothing
                                         Just (a,rest) -> parse (f a) rest


-- | parse one thing, if that works then parse the other thing
-- return both result in a pair
-- if either of the parsers fail, the result fails
(+++) :: Parser a  -- ^ first parse with this parser
        -> Parser b  -- ^ then parse with this parser
        -> Parser (a,b)  -- ^ return the results of the two parsers in a pair
pa +++ pb =  do a <- pa; b <- pb; return (a,b)

-- | map the function f to the result of the Parser
mapParser :: Parser a -> (a->b) -> Parser b
mapParser pa f = fmap f pa

-- | just read a char (from book)
-- and return the char itself
--
-- >>> parse item "any string"
-- Just ("a", "ny string")
item :: Parser Char
item = Parser $ \ input -> case input of ""    -> Nothing
                                         (h:t) -> Just (h, t)


-- | a parser that always fails (empty in the book)
failParse :: Parser a
failParse = Parser $ \ input -> Nothing


-- | read in a char if it satisfy some property (from book)
--
-- >>> parse (sat isDigit) "456"
-- Just ("4", "56")
--
-- >>> parse (sat isDigit) "any string"
-- Nothing
sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c
           then return c
           else failParse


-- | parse exactly a string, return that string (in book as the poorly named "string")
--
-- >>> parese (literal "test") "test1"
-- Just ("test", "1")
--
-- >>> parese (literal "test") "hello"
-- Nothing
literal :: String  -- ^ the string to match
          -> Parser String  -- ^ the parser that match exactly the input string
literal "" = return ""
literal (h:t) = do sat (==h)
                   literal t
                   return (h:t)


-- | try to parse a, if that doesn't work try to parse b (slightly different from the book)
-- then return either the first result (if the first parse is successful)
-- or the second result (if the first parse is not successful and the second is successful)
-- in a either type.
-- 
-- if both are not successful then fail.
(<||>) :: Parser a  -- ^ try this parser first
        -> Parser b  -- ^ if the first parser fails, try this parser
        -> Parser (Either a b)  -- ^ return either the first of the second
parserA <||> parserB = Parser $ \ input ->  case parse parserA input of
                                                 Just (a, rest) -> Just (Left a, rest)
                                                 Nothing -> case parse parserB input of
                                                              Just (b, rest) -> Just (Right b, rest)
                                                              Nothing        -> Nothing

-- | like <||> but easier on the same type (from book)
(<|>) :: Parser a -> Parser a -> Parser a
l <|> r =
  do res <- l <||> r
     case res of
       Left  a -> return a
       Right a -> return a


-- | take a parser and parse as much as possible into a list, always parse at least 1 thing, (from book)
some :: Parser a -> Parser ([a])
some pa = do a <- pa
             rest <- rep pa
             return (a:rest)



-- | take a parser and parse as much as possible into a list, (in book as "many")
rep :: Parser a -> Parser ([a])
rep pa =  do res <- (some pa) <||> (return [])
             case res of Left ls  -> return ls
                         Right ls -> return ls


-- | parse a digit (from book)
digit :: Parser Char
digit = sat isDigit


-- | parse natural numbers, like "123", or "000230000"
natParser :: Parser Integer
natParser =  do digits <- some digit
                return $ read digits

-- | parse an integer
intParser  :: Parser Integer
intParser = do r <- (literal "-") <||> natParser
               case r of
                Left _ -> fmap (0-) natParser
                Right n -> return n

-- | parse a floating-point number
floatParser :: Parser Float
floatParser = do sgn <- (literal "-") <||> (rep digit)
                 case sgn of
                   Left _ -> do x <- (rep digit)
                                _ <- (literal ".")
                                y <- (rep digit)
                                return $ 0 - (read (x ++ "." ++ y) :: Float)
                   Right x -> do (literal ".")
                                 y <- (rep digit)
                                 return $ (read (x ++ "." ++ y) :: Float)

-- | parse spaces, throw them away
spaces :: Parser ()
spaces =  do rep (sat isSpace)
             rep ((literal "{-") +++ (rep (sat (notSymbol))) +++ (literal "-}") +++ spaces)
             rep ((literal "--") +++ (rep (sat (/= '\n'))) +++ (literal "\n") +++ spaces)
             return ()

-- | Parses continuous strings
stringParser :: Parser String
stringParser = do x <- rep (sat (/= '\"'))
                  token $ literal "\""
                  return x

-- | Parses the not symbol
notSymbol  :: Char -> Bool
notSymbol c = (isSpace c) || (isAlpha c)

-- | a nicer version of eat spaces, eat the spaces before or after the parser (from book)
token :: Parser a -> Parser a
token pa = do spaces
              a <- pa
              spaces
              return a

-- | parse what we will consider a good variable name
varParser :: Parser String
varParser =
  do head <- sat isAlpha
     tail <- rep (sat isAlpha <|> digit <|>  sat (=='\''))
     return $ head : tail


-- | use the first working parser
oneOf :: [Parser a] -> Parser a
oneOf [] = failParse
oneOf (pa:rest) = pa <|> oneOf rest


-- | handle infix parsing with left associativity
withInfix :: Parser a -> [(String, a -> a -> a)] -> Parser a
withInfix pa ls = let operators = fmap fst ls
                      opParsers = fmap (\ s -> token $ literal s) operators

                      --innerParser :: a -> Parser a, where a is the same as above
                      innerParser left = do s <- oneOf opParsers
                                            next <- pa
                                            case lookup s ls of
                                              Nothing -> failParse
                                              Just f ->  let out = f left next
                                                         in (innerParser out) <|> return out
                   in do l <- pa
                         (innerParser l) <|> (return l)

--withInfix' :: Parser a -> [(String, a -> a -> a)] -> Parser a
--withInfix' pa ls = let operators = fmap fst ls
--                      opParsers = fmap (\ s -> token $ literal s) operators

--                      --innerParser :: a -> Parser a, where a is the same as above
--                      innerParser left = do s <- oneOf opParsers
--                                            case s of
--                                              "--" -> do rep (sat (/= '\n'))
--                                                         next <- pa
--
--                                            next <- pa
--                                            case lookup s ls of
--                                              Nothing -> failParse
--                                              Just f ->  case f of
--                                                           Comment ->
--                                                             do rep (sat (/= '\n'))

--                                                let out = f left next
--                                                         in (innerParser out) <|> return out
--                   in do l <- pa
--                         (innerParser l) <|> (return l)
