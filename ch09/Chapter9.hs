module Chapter9 where

readLine :: IO String
readLine = get ""
           where
            get xs = do x <- getChar
                        case x of
                          '\n' -> return xs
                          '\DEL' -> if null xs then
                                      get xs
                                    else
                                      do putStr "\ESC[1D \ESC[1D"
                                         get (init xs)
                          _ -> get (xs ++ [x])
