last1 = head . reverse

last2 xs = head $ drop (length xs - 1) xs

last3 xs = xs !! (length xs - 1)
