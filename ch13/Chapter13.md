1)

```haskell
last :: [a] -> a
last [x] = x
last (_:xs) = last xs
```

2)

```haskell
add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)
```

What needs to be proved: `add n (Succ m) = Succ (add n m)`

Base case:

```haskell
add Zero (Succ m)
-- applying `add`
Succ m
-- unapplying `add`
Succ (add Zero m)
```

Inductive case:

```haskell
add (Succ n) (Succ m)
-- applying `add`
Succ (add n (Succ m))
-- induction hypothesis
Succ (Succ (add n m))
-- unapplying `add`
Succ (add (Succ n) m)
```

3)

Property: `add n Zero = n`

What needs to be proved: `add n m = add m n`

Base case:

```haskell
add Zero m
-- applying `add`
m
-- property of `add`
add m Zero
```

Inductive case:

```haskell
add (Succ n) m
-- applying `add`
Succ (add n m)
-- induction hypothesis
Succ (add m n)
-- property of `add`
add m (Succ n)
```

4)

```haskell
all p [] = True
all p (x:xs) = p x && all p xs
```

What needs to be proved: `all (== x) (replicate n x)`

Base case:

```haskell
all (== x) (replicate 0 x)
-- applying `replicate`
all (== x) []
-- applying `all`
True
```

Inductive case:

```haskell
all (== x) (replicate (n + 1) x)
-- applying `replicate`
all (== x) (x : replicate n x)
-- applying `all`
x == x && all (== x) (replicate n x)
-- applying `==`
True && all (== x) (replicate n x)
-- applying `&&`
all (== x) (replicate n x)
-- induction hypothesis
True
```

5)

```haskell
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
```

What needs to be proved: `xs ++ [] = xs`

Base case:

```haskell
[] ++ []
-- applying `++`
[]
```

Inductive case:

```haskell
(x:xs) ++ []
-- applying `++`
x : (xs ++ [])
-- induction hypothesis
x:xs
```

What needs to be proved: `xs ++ (ys ++ zs) = (xs ++ ys) ++ zs`

Base case:

```haskell
[] ++ (ys ++ zs)
-- applying `++`
ys ++ zs
-- unapplying `++`
([] ++ ys) ++ zs
```

Inductive case:

```haskell
(x:xs) ++ (ys ++ zs)
-- applying `++`
x : (xs ++ (ys ++ zs))
-- induction hypothesis
x : ((xs ++ ys) ++ zs)
-- unapplying `++`
(x:(xs ++ ys)) ++ zs
-- unapplying `++`
((x:xs) ++ ys) ++ zs
```

6) Three auxiliary results might be more useful in different contexts while just
one is specific to a single application.

7)

```haskell
map f [] = []
map f (x:xs) = f x : map f xs

(f . g) x = f (g x)
```

What needs to be proved: `map f (map g xs) = map (f . g) xs`

Base case:

```haskell
map f (map g [])
-- applying inner `map`
map f []
-- applying `map`
[]
-- unapplying `map`
map (f . g) []
```

Inductive case:

```haskell
map f (map g (x:xs))
-- applying inner `map`
map f (g x : map g xs)
-- applying outer `map`
f (g x) : (map f (map g xs))
-- property of `f (g x)`
(f . g) x : (map f (map g xs))
-- induction hypothesis
(f . g) x : map (f . g) xs
-- unapplying `map`
map (f . g) (x:xs)
```

8)

```haskell
take 0 _ = []
take (n + 1) [] = []
take (n + 1) (x:xs) = x : take n xs

drop 0 xs = xs
drop (n + 1) [] = []
drop (n + 1) (_:xs) = drop n xs
```

What needs to be proved: `take n xs ++ drop n xs = xs`

Base case for `n = 0`:

```haskell
take 0 xs ++ drop 0 xs
-- applying `take` and `drop`
[] ++ xs
-- applying `++`
xs
```

Base case for `xs = []`:

```haskell
take (n + 1) [] ++ drop (n + 1) []
-- applying `take` and `drop`
[] ++ []
-- applying `++`
[]
```

Inductive case:

```haskell
take (n + 1) (x:xs) ++ drop (n + 1) (x:xs)
-- applying `take` and `drop`
(x : take n xs) ++ drop n xs
-- applying `++`
x : (take n xs ++ drop n xs)
-- induction hypothesis
x:xs
```

9)

```haskell
data Tree = Leaf Int | Node Tree Tree

leaves :: Tree -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

nodes :: Tree -> Int
nodes (Leaf _) = 0
nodes (Node l r) = 1 + nodes l + nodes r
```

What needs to be proved: `leaves t = nodes t + 1`

Base case:

```haskell
nodes (Leaf n) + 1
-- applying `nodes`
0 + 1
-- applying `+`
1
-- unapplying `leaves`
leaves (Leaf n)
```

Inductive case:

```haskell
nodes (Node l r) + 1
-- applying `nodes`
1 + (nodes l) + (nodes r) + 1
-- property of `+`
(nodes l + 1) + (nodes r + 1)
-- induction hypothesis
leaves l + leaves r
-- unapplying `leaves`
leaves (Node l r)
```

10)

```haskell
comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]
```

Property: `comp' e c = comp e ++ c`

Base case:

```haskell
comp' (Val n) c
-- applying `comp'`
comp (Val n) ++ c
-- applying `comp`
[PUSH n] ++ c
-- applying `++`
PUSH n : c
```

Inductive case:

```haskell
comp' (Add x y) c
-- applying `comp'`
comp (Add x y) ++ c
-- applying `comp`
comp x ++ comp y ++ [ADD] ++ c
-- associativity of `++`
comp x ++ (comp y ++ ([ADD] ++ c))
-- applying inner `++`
comp x ++ (comp y ++ (ADD : c))
-- induction hypothesis for `y`
comp x ++ (comp' y (ADD : c))
-- induction hypothesis for `x`
comp' x (comp' y (ADD : c))
```

```haskell
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))
```
