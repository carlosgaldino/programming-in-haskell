module Chapter7 where

-- Exercise 1
-- [f x | x <- xs, p x]
mapterhension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapterhension f p = map f . filter p

-- Exercise 2
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) = f x && all' f xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
  | f x = dropWhile' f xs
  | otherwise = (x:xs)

-- Exercise 3
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x xs -> f x : xs) []

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x xs -> if p x then x:xs else xs) []

-- Exercise 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- Exercise 5
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- sumsqreven = compose [sum, map (** 2), filter even]
-- The definition above is invalid because the functions in the list do not
-- have all the same type.
