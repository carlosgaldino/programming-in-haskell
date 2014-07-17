module Chapter4 where

-- Exercise 1
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Exercise 2
safeTail :: [a] -> [a]
safeTail xs = if (null xs) then [] else drop 1 xs

safeTail2 :: [a] -> [a]
safeTail2 xs
  | null xs   = []
  | otherwise = drop 1 xs

safeTail3 :: [a] -> [a]
safeTail3 [] = []
safeTail3 (_:xs) = xs

-- Exercise 3
orr :: Bool -> Bool -> Bool
orr True _ = True
orr _ True = True
orr _ _    = False

orr2 :: Bool -> Bool -> Bool
orr2 True _  = True
orr2 False b = b

orr3 :: Bool -> Bool -> Bool
orr3 True True   = True
orr3 True False  = True
orr3 False True  = True
orr3 False False = False

orr4 :: Bool -> Bool -> Bool
orr4 False False = False
orr4 _ _         = True

-- Exercise 4
andd :: Bool -> Bool -> Bool
andd x y = if x then
                if y then True
                     else False
                else False

-- Exercise 5
andd2 :: Bool -> Bool -> Bool
andd2 x y = if x then y else False

-- Exercise 6
mult :: (Num a) => a -> a -> a -> a
mult = \x -> \y -> \z -> x * y * z
