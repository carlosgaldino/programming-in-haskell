init' xs = take (length xs - 1) xs

init'' = reverse . tail . reverse
