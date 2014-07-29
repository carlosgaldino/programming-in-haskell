module Chapter7 where

import Data.Char (ord, chr)

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

-- Exercise 6
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

-- Exercise 7
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold f = unfold null (f . head) tail

iterateUnfold :: (a -> a) -> a -> [a]
iterateUnfold f = unfold (const False) id f

-- Exercise 8
bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
                 where weights = iterateUnfold (*2) 1

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

encodeWithParity :: String -> [Bit]
encodeWithParity = concat . map (addParityBit . make8 . int2bin . ord)

decodeWithParity :: [Bit] -> String
decodeWithParity = map (chr . bin2int . checkParityBit) . chop9

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

addParityBit :: [Bit] -> [Bit]
addParityBit bits = (parity bits) : bits

parity :: [Bit] -> Bit
parity bits
  | odd (sum bits) = 1
  | otherwise = 0

checkParityBit :: [Bit] -> [Bit]
checkParityBit (b:bs)
  | b == parity bs = bs
  | otherwise = error "Wrong Parity Bit"

transmit' :: String -> String
transmit' = decodeWithParity . faultyChannel . encodeWithParity

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

transmit'' :: String -> String
transmit'' = decodeWithParity . channel . encodeWithParity
