module Chapter5 where

import Data.Char (isLower, isUpper, isLetter, chr, ord, toLower, toUpper)

-- Exercise 1
summ :: IO ()
summ = do
    print (sum [x ^ 2 | x <- [1..100]])

-- Exercise 2
replicatee :: Int -> a -> [a]
replicatee n x = [x | _ <- [1..n]]

-- Exercise 3
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

-- Exercise 4
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sumFactors x == x]
             where sumFactors = sum . init . factors

{-- Exercise 5
  concat [[(x, y) | x <- [1..3]] | y <- [4..6]]
--}

-- Exercise 6
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- Exercise 7
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Exercise 8
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = toUpper $ shift n $ toLower c
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = letters xs

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

letters :: String -> Int
letters xs = length [x | x <- xs, isLetter x]

chisqr :: [Float ] -> [Float ] -> Float
chisqr os es = sum [((o - e) ** 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs
