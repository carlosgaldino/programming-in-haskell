module Chapter11 where

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
               where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- Exercise 1
choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs,
                   zs <- perms ys]

-- Exercise 2
removeOne :: Eq a => a -> [a] -> [a]
removeOne x [] = []
removeOne x (y:ys)
  | x == y    = ys
  | otherwise = y : removeOne x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys
  | x `elem` ys = isChoice xs (removeOne x ys)
  | otherwise   = False
