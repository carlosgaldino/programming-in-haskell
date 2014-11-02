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

data Op = Add | Sub | Mul | Div | Exp

data Expr = Val Int | App Op Expr Expr

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = True
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0
valid Exp _ _ = True

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns
               , l       <- exprs ls
               , r       <- exprs rs
               , e       <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns
                    , e   <- exprs ns'
                    , eval e == [n]]

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]

-- Exercise 4 and 5
main :: IO ()
main = do
    putStrLn . show $ length [e | ns <- choices [1, 3, 7, 10, 25, 50], e <- exprs ns]
    -- 33665406
    putStrLn . show $ length [e | ns <- choices [1, 3, 7, 10, 25, 50], e <- exprs ns, eval e /= []]
    -- 4672540
    putStrLn . show $ length [e | ns <- choices [1, 3, 7, 10, 25, 50], e <- exprs ns, eval e /= []]
    -- 10839369
