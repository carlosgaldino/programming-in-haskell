module Chapter6 where

-- Exercise 1
pow :: Int -> Int -> Int
pow x 1 = x
pow x y = x * (pow x (pred y))

{--
2 pow 3
= { applying pow }
2 * (2 pow 2)
= { applying pow }
2 * (2 * (2 pow 1))
= { applying pow }
2 * (2 * 2)
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
elem' x [] = False
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
