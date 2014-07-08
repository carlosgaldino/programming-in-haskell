```
qsort [2, 2, 3, 1, 1]
= { applying qsort }
qsort [1, 1] ++ [2] ++ qsort [3]
= { applying qsort }
([] ++ [1] ++ []) ++ [2] ++ ([] ++ [3] ++ [])
= { applying ++ }
[1] ++ [2] ++ [3]
= { applying ++ }
[1, 2, 3]
```

Which can then be viewed as a sorted list with unique elements.
