{- |
Module: Parser
Description: Parser for Avengers: Infinity List language
-}
module Parser where

import Ast
import ParserMonad
import EnvUnsafeLog

-- | parser for the language
parser :: Parser Ast
parser = apps
-- | keywords that cannot be used as variable names or other sensitive strings
keywords = ["if","then","else", "let", "in", "true","false"]
-- | Parser for the App function using withInfix. Linked to hierarchy step in AST seps
apps :: Parser Ast
apps = withInfix seps [("",App)] -- the tokens eat up all the spaces so we split on the empty string
-- | Parser for the Seperator function. Linked to next step in hierarchy, compose.
seps :: Parser Ast
seps = seps' <|> compose

-- | right associative implementation of seps, parses for two `compose` level values seperated with an ";"
seps' :: Parser Ast
seps' = do x <- (token compose)
           token (literal ";")
           y <- (token seps)
           return (Separator x y)

-- | parser for the infix function composition operator (Mix-In: 5 Points)
compose = withInfix cons [(".", Compose)]
-- | cons operator that holds all varieties of possible list parsers
cons :: Parser Ast
cons = brackets <|> cons' <|> listConcatParser

-- | right associative cons parser that accounts for two elements of a list seperated by a ":". Returns in (Cons x y) format.
cons' :: Parser Ast
cons' = do x <- (token listConcatParser)
           token (literal ":")
           y <- (token cons) <|> nil
           case y of (Cons _ _) -> return (Cons x y)
                     Nil        -> return (Cons x y)
                     _          -> return (Cons x (Cons y Nil))
-- | brackets parser that eliminates the first and last brackets, with a secondary cons'' parser that will parse the contents
brackets :: Parser Ast
brackets = do (token $ literal "[")
              x <- cons''
              (token $ literal "]")
              case x of (Cons _ _) -> return x
                        _    -> return (Cons x Nil)

-- | cons'' parser that holds a commaParser and the next hierarchy, listConcatParser.
cons'' :: Parser Ast
cons'' = commaParser <|> listConcatParser
-- | commaParser that parses orExpr values separated by commas, to be returned as a list in (Cons x y) format
commaParser :: Parser Ast
commaParser = do x <- token listConcatParser
                 token $ literal ","
                 y <- token cons'' <|> nil
                 case y of (Cons _ _) -> return (Cons x y)
                           Nil        -> return (Cons x y)
                           _          -> return (Cons x (Cons y Nil))

-- | parses List Concatenation expressions using the withInfix function. Links to next Parser Ast orExpr
listConcatParser :: Parser Ast
listConcatParser = withInfix orExpr [("++", ListConcat)]
-- | parses Or expressions using the withInfix function. Links to next Parser AST andExpr.
orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]
-- | parses And expressions using the withInfix function. Links to the next Parser AST comparison.
andExpr :: Parser Ast
andExpr = withInfix comparison [("&&", And)]
-- | parses all Comparison expressions Equals, NotEquals, LessThanOrEquals, LessThan, GreaterThanOrEquals, and GreaterThan using the withInfix function. Links to the next Parser AST addSubExpr
comparison :: Parser Ast
comparison = withInfix addSubExpr [("==", Equals), ("/=", NotEquals), ("<=", LessThanOrEquals), ("<", LessThan), (">=" , GreaterThanOrEquals), (">", GreaterThan)]
-- | parses addition and subtraction expressions "+" "-" using the withInfix function. Links to the next Parser Ast multDivExpr.
addSubExpr :: Parser Ast
addSubExpr = withInfix multDivOrModExpr [("+", Plus),("-", Minus)]
-- | parses multiply divide and modulus expressions "*" "/" "//" "%" using the withInfix function. Links to the next Parser Ast expExpr.
multDivOrModExpr :: Parser Ast
multDivOrModExpr = withInfix expExpr [("*", Mult), ("//", IntDiv), ("/", FloatDiv), ("%", Mod)]
-- | parses exponent expressions "**" "^" using the withInfix function. Links to the next Parser Ast negExp'
expExpr :: Parser Ast
expExpr = withInfix negExp' [("**", IntExp), ("^", FloatExp)]
-- | negExp parses the negative operator "-", with the next Parser Ast level listIndexExpr
negExp :: Parser Ast
negExp = do token (literal "-")
            x <- (token listIndexExpr)
            return (NegExp x)
-- | linking negExp to listIndexExpr if negExp fails
negExp' :: Parser Ast
negExp' = negExp <|> listIndexExpr
-- | parses list index operator "!!" using the withInfix function. Links to the next Parser Ast prefixExpr
listIndexExpr :: Parser Ast
listIndexExpr = withInfix prefixExpr [("!!", ListIndex)]
-- | prefixExpr links the notExp, printExp and atoms together
prefixExpr :: Parser Ast
prefixExpr = notExp <|> printExp <|> atoms
-- | parses print operator "print(_)" that can have any parser inside of it.
printExp :: Parser Ast
printExp = do token (literal "print(")
              x <- parser
              token (literal ")")
              return (Print x)
-- | links all the following "atom" elements inorder
atoms:: Parser Ast
atoms = floats <|> ints <|> chars <|> strings <|> bools  <|>  nil <|> parens <|> ifParser <|> letParser <|>  lambdaParser <|> vars
-- | parses the boolean not operator "!", which can include a prefixExpr in its body. Returns (Not x)
notExp :: Parser Ast
notExp = do token (literal "not")
            x <- (token prefixExpr)
            return (Not x)
-- | parses the next variable using varParser, returns as Var.
vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s
-- | parses the next integer using intParser, returns as ValInt.
ints :: Parser Ast
ints = do s <- token $ intParser
          return (ValInt s)
-- | parses the next float using floatParser, returns as ValFloat.
floats :: Parser Ast
floats = do s <- token $ floatParser
            return (ValFloat s)
-- | parses the next char using item, returns as ValChar.
chars :: Parser Ast
chars = do token $ literal "'"
           s <- token $ item
           token $ literal "'"
           return $ (ValChar s)
-- | parses the next string using stringParser, which also works to parse strings. Returns as ValString
strings :: Parser Ast
strings = do token $ literal "\""
             s <- stringParser
             return (ValString s)

-- | parses the next bool using varParser, comparing it to "true" or "false" to return as ValBool
bools :: Parser Ast
bools = do s <- token $ varParser
           if (s == "true" || s == "false")
           then case s of
             "true" -> return (ValBool True)
             "false" -> return (ValBool False)
             otherwise -> failParse --should not happen
           else failParse

-- | parses nil list value
nil :: Parser Ast
nil = do token $ literal "[]"
         return Nil
-- | parses if-then-else statements, which can have any value inside of it
ifParser :: Parser Ast
ifParser = do token $ literal "if"
              condition <- parser
              token $ literal "then"
              case1 <- parser
              token $ literal "else"
              case2 <- parser
              return (If condition case1 case2)

-- | parses let = in statements, which must take in a "let" for the first value, where the next parse value is the definitions
letParser :: Parser Ast
letParser = do token $ literal "let"
               x <- letDefinitions
               return x
-- | parses the body of the let statement
letFinish :: Parser Ast
letFinish = do token $ literal "in"
               ast <- parser
               return ast
-- | parses the = in each variable defintion inside of let functions.
letDefinitions :: Parser Ast
letDefinitions = do label <- token varParser
                    token $ literal "="
                    assigned <- parser
                    y <- nextLet <|> letFinish
                    return $ Let label assigned y

-- | parses the comma between variables in multi-definition let functions
nextLet :: Parser Ast
nextLet = do token $ literal ","
             x <- letDefinitions
             return x

-- | parses lambda functions, which must take in a lambda variable, or multiple variables
lambdaParser :: Parser Ast
lambdaParser = do token $ literal "\\"
                  x <- lambdaVarParser
                  return x
-- | parses the arrow that indicates the body of the lambda, which can hold any parsable value
lambdaFinish :: Parser Ast
lambdaFinish = do token $ literal "->"
                  ast <- parser
                  return ast
-- | parses the next lambda variable, or the lambda finish (body) of the lambda function.
lambdaVarParser :: Parser Ast
lambdaVarParser = do x <- token varParser
                     y <- lambdaVarParser <|> lambdaFinish
                     return $ Lam x y
-- | parses all the parentheses before and after any parser operation.
parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast

