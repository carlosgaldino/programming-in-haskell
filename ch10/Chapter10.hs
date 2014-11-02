module Chapter10 where

data Nat = Zero | Succ Nat
           deriving (Show)

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (pred n))

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

-- Exercise 1
mult :: Nat -> Nat -> Nat
mult x Zero     = Zero
mult m (Succ n) = add m (mult m n)

-- Exercise 2
data Tree' = Leaf' Int | Node' Tree' Int Tree'

occurs :: Int -> Tree' -> Bool
occurs n (Leaf' m)     = n == m
occurs n (Node' l m r) = case compare n m of
                           LT -> occurs n l
                           EQ -> True
                           GT -> occurs n r

-- This version is more efficient because it will only search one side of
-- the tree instead of searching both sides as the initial version on the
-- book did.

-- Exercise 3
data Tree = Leaf Int | Node Tree Tree
            deriving (Show)

leaves :: Tree -> Int
leaves (Leaf _)   = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree -> Bool
balanced (Leaf _)   = True
balanced (Node l r) = abs (lcount - rcount) <= 1 && balanced l && balanced r
                        where lcount = leaves l
                              rcount = leaves r

-- Exercise 4
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [Int] -> Tree
balance [x] = Leaf x
balance xs  = Node (balance l) (balance r)
                where (l, r) = halve xs

-- Exercise 8
instance Monad Maybe where
    (Just x) >>= g = g x
    Nothing  >>= _ = Nothing

    return = Just

instance Monad [] where
    return x = [x]

    x >>= y = foldr ((++) . y) [] x
