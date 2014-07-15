product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

main = do
  putStrLn . show $ product' [2, 3, 4]
