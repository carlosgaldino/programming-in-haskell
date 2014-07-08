revqsort [] = []
revqsort (x:xs) = revqsort larger ++ [x] ++ revqsort smaller
                  where
                    smaller = [a | a <- xs, a <= x]
                    larger  = [a | a <- xs, a > x]
