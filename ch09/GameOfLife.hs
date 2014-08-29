module GameOfLife where

import Data.List ((\\))

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

type Pos = (Int, Int)

width :: Int
width = 5

height :: Int
height = 5

type Board = [Pos]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "o" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x-1,y-1), (x,y-1),
                           (x+1,y-1), (x-1,y),
                           (x+1,y)  , (x-1,y+1),
                           (x,y+1)  , (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height + 1))

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do showcells b
            wait 50000
            clearDeads (b \\ survivors b)
            life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

clearDeads :: Board -> IO ()
clearDeads b = sequence_ [writeat p " " | p <- b]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

blinker :: Board
blinker = [(3, 2), (3, 3), (3, 4)]
