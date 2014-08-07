module Chapter8 where

import Parsing

-- Since `int` is not defined inside `Parsing` we need to define `integer`
-- here.
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
             return ()
