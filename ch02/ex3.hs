-- the errors were:
--
-- N was uppercase
-- It wasn't using backticks
-- The indentation of `xs` was wrong
n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]
