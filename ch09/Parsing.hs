module Parsing (parse, expr) where

import Data.Char
import Control.Monad

newtype Parser a = P (String -> [(a, String)])

instance Monad Parser where
    return v = P (\inp -> [(v, inp)])
    p >>= f  = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> parse (f v) out)

instance MonadPlus Parser where
    mzero       = P (\inp -> [])
    p `mplus` q = P (\inp -> case parse p inp of
                             [] -> parse q inp
                             [(v, out)] -> [(v, out)])

failure :: Parser a
failure = mzero

item :: Parser Char
item = P (\inp -> case inp of
                  [] -> []
                  (x:xs) -> [(x, xs)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

infixr 5 +++

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

integer :: Parser Int
integer = token int

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       +++ nat

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           +++ do symbol "-"
                  e <- expr
                  return (t - e)
           +++ return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           +++ do symbol "/"
                  t <- term
                  return (f `div` t)
           +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          +++ integer
