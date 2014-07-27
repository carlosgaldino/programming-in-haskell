module Chapter6 where

-- Exercise 1
pow :: Int -> Int -> Int
pow _ 0 = 1
pow x y = x * (pow x (pred y))

{--
2 pow 3
= { applying pow }
2 * (2 pow 2)
= { applying pow }
2 * (2 * (2 pow 1))
= { applying pow }
2 * (2 * (2 * (2 pow 0)))
= { applying pow }
2 * (2 * (2 * 1))
= { applying * }
8
--}

-- Exercise 2

{--
length [1, 2, 3]
= { applying length }
1 + length [2, 3]
= { applying length }
1 + (1 + length [3])
= { applying length }
1 + (1 + (1 + length []))
= { applying length }
1 + (1 + (1 + 0))
= { applying + }
3
--}

{--
drop 3 [1, 2, 3, 4, 5]
= { applying drop }
drop 2 [2, 3, 4, 5]
= { applying drop }
drop 1 [3, 4, 5]
= { applying drop }
drop 0 [4, 5]
= { applying drop }
[4, 5]
--}

{--
init [1, 2, 3]
= { applying init }
1 : init [2, 3]
= { applying init }
1 : 2 : init [3]
= { applying init }
1 : 2 : []
= { applying : }
[1, 2]
--}

-- Exercise 3
and' :: [Bool] -> Bool
and' [] = True
and' (False:_) = False
and' (True:xs) = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (pred n) x

nth :: [a] -> Int -> a
nth (x:_) 0 = x
nth (_:xs) n = nth xs (pred n)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = elem' x ys

-- Exercise 4
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Exercise 5
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort l) (msort r)
  where (l, r) = halve xs

-- Exercise 6

{--
-- Step 1: define the type
sum' :: Num a => [a] -> a

-- Step 2: enumerate the cases
sum' []     =
sum' (x:xs) =

-- Step 3: define the simple cases
sum' []     = 0
sum' (x:xs) =

-- Step 4: define the other cases
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- Step 5: generalise and simplify
sum' = foldl (+) 0
--}

sum' :: Num a => [a] -> a
sum' = foldl (+) 0

{--
-- Step 1: define the type
take' :: Int -> [a] -> [a]

-- Step 2: enumerate the cases
take' 0 []     =
take' 0 (x:xs) =
take' n []     =
take' n (x:xs) =

-- Step 3: define the simple cases
take' 0 []     = []
take' 0 (x:xs) = []
take' n []     = []
take' n (x:xs) =

-- Step 4: define the other cases
take' 0 []     = []
take' 0 (x:xs) = []
take' n []     = []
take' n (x:xs) = x : take' (pred n) xs

-- Step 5: generalise and simplify
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (pred n) xs
--}

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (pred n) xs

{--
-- Step 1: define the type
last' :: [a] -> a

-- Step 2: enumerate the cases
last' [x]    =
last' (x:xs) =

-- Step 3: define the simple cases
last' [x]    = x
last' (x:xs) =

-- Step 4: define the other cases
last' [x]    = x
last' (x:xs) = last' xs

-- Step 5: generalise and simplify
last' [x]    = x
last' (_:xs) = last' xs
--}

last' :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs
