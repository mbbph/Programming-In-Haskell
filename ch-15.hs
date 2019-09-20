--1
--a. 2*3 (both)
--b. 1+2 & 2+3 (inner, neither)
--c. 1+2, 2+3 (inner, neither), fst (1+2) (outer)
--d. 2*3 (inner), (\(2*3) -> 1 + (2*3)) (outer)

--2
--The second value of the pair doesn't make it into the final result so it's wasteful to perform innermost evaluation and evaluate 2+3.

--3
-- mult = \x -> (\y -> x * y)
-- mult 3 4 = \x -> (\y -> x * y) 3 4
-- = \3 -> (\y -> x * y)
-- = (\y -> 3 * y) 4
-- = (\4 -> 3 * y)
-- = 3 * 4
-- = 12

--4
toAdd (a,b) = a + b
fibbs = 0:1:[toAdd x | x <- zip (fibbs) (tail fibbs)]

--5
--generates an infinite tree
repeat :: a -> Tree a
repeat x = Node (repeat x) x (repeat x)

--"Trims" a tree to have n levels (and just discard the leftover branches??)
take :: Int -> Tree a -> Tree a
take 0 _ = Leaf
take _ Leaf = Leaf
take 1 (Node l x r) = Node Leaf x Leaf --returns 1 tree...this case is prob excessive
take n (Node l x r) = Node (take (n-1) l) x (take (n-1) r)

--Generates a balanced tree with n levels
replicate :: Int -> a -> Tree a
replicate n = take n . repeat

--6

sqroot :: Double -> Double
sqroot n = snd (getPair n)

--helper function that gets the first pair of approximations that satisfy the condition
getPair :: Double -> (Double, Double)
getPair n = head [(x,y) | (x:y:_) <- tails (iterate (\x -> (x + n/x)/2) 1.0), abs (x-y) < 0.00001]

--alternative ways of generating list of consecutive pairs from stack overflow:
--head $ zip xs $ tail xs where xs = iterate (\x -> (x + n/x)/2) 1.0
--zip <*> tail
