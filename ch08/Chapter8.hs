module Chapter8 where

import Parsing

-- Since `int` is not defined inside `Parsing` we need to define `integer` here.
integer :: Parser Int
integer = token int

-- Exercise 1
int :: Parser Int
int = do char '-'
         n <- nat
         return (negate n)
       +++ nat

-- Exercise 2
comment :: Parser ()
comment = do symbol "--"
             many (sat (/= '\n'))
             char '\n'
             return ()

-- Exercise 6
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
          +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])] -> n
            [(_, out)] -> error ("unused input " ++ out)
            [] -> error "invalid input"

-- Exercise 7
factor' :: Parser Int
factor' = do a <- atom
             do symbol "^"
                f <- factor'
                return (a ^ f)
              +++ return a

atom :: Parser Int
atom = do symbol "("
          e <- expr
          symbol ")"
          return e
        +++ natural

expr' :: Parser Int
expr' = do t <- term'
           do symbol "+"
              e <- expr'
              return (t + e)
            +++ do symbol "-"
                   e <- expr'
                   return (t - e)
            +++ return t

term' :: Parser Int
term' = do f <- factor'
           do symbol "*"
              t <- term'
              return (f * t)
            +++ do symbol "/"
                   t <- term'
                   return (f `div` t)
            +++ return f

eval' :: String -> Int
eval' xs = case parse expr' xs of
             [(n, [])] -> n
             [(_, out)] -> error ("unused input " ++ out)
             [] -> error "invalid input"

-- Exercise 8
expr'' :: Parser Int
expr'' = do e <- expr''
            symbol "-"
            n <- natural
            return (e - n)
          +++ natural

{--
The problem with this parser is that it loops forever without ever producing a
result of its left part that is being called recursively.
--}

expr''' :: Parser Int
expr''' = do n <- natural
             ns <- many (do symbol "-"
                            natural)
             return (foldl (-) n ns)

eval'' :: String -> Int
eval'' xs = case parse expr''' xs of
              [(n, [])] -> n
              [(_, out)] -> error ("unused input " ++ out)
              [] -> error "invalid input"
