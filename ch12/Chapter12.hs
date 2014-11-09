module Chapter12 where

-- Exercise 1
{--
  1 + (2 * 3)
    2 * 3 -> both

  (1 + 2) * (2 + 3)
    1 + 2 -> innermost
    2 + 3

  fst (1 + 2, 2 + 3)
    1 + 2 -> innermost
    2 + 3
    fst (1 + 2, 2 + 3) -> outermost

  (\x -> 1 + x)(2 * 3)
    2 * 3 -> innermost
    (\x -> 1 + x)(2 * 3) -> outermost
--}

-- Exercise 2
fst :: (a, b) -> a
fst (x, _) = x

{-- outermost application
  fst (1 + 2, 2 + 3)
=   { applying fst }
  1 + 2
=   { applying + }
  3
--}

{-- innermost application
  fst (1 + 2, 2 + 3)
=   { applying + }
  fst (3, 2 + 3)
=   { applying + }
  fst (3, 5)
=   { applying fst }
  3
--}

{--
  Outermost application is better because it requires less reduction steps since
  it does not evaluate the second value of the tuple.
--}

-- Exercise 3
{--
  mult = \x -> (\y -> x * y)

  mult 3 4
=   { applying mult }
  (\x -> (\y -> x * y)) 3 4
=   { applying \x -> (\y -> x * y) }
  (\y -> 3 * y) 4
=   { applying \y -> 3 * y }
  3 * 4
=   { applying * }
  12
--}

-- Exercise 4
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

-- Exercise 5
fib :: Int -> Integer
fib n = fibs !! n

-- head $ dropWhile (< 1000) fibs

-- Exercise 6
data Tree a = Leaf | Node (Tree a) a (Tree a)
            deriving (Show)

repeatTree :: a -> Tree a
repeatTree x = Node t x t
                where t = repeatTree x

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _            = Leaf
takeTree x Leaf         = Leaf
takeTree x (Node l y r) = Node (takeTree (x - 1) l) y (takeTree (x - 1) r)

replicateTree :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree
