--1
instance Functor Tree where
  fmap g Leaf = Leaf
  fmap g (Node l n r) = Node (fmap g l) (g x) (fmap g r)

--2
instance Functor ((->) a) where
  fmap = (.)

--3
instance Applicative ((->) a) where
  pure = const

--4
instance Functor ZipList where
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  pure x = Z (repeat x)
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

--5  Not too sure how to answer this, but here are my thoughts: 
  --a. x is some container object
  --b. x is some container object
  --c. y is a function that takes another function
  --   x is the function applied to y
  --   g is the placeholder for x
  --4. z is some container object, and x and y have types (b -> c) and (a -> b) 

--6
instance Monad ((->) a) where
  return = pure
  mx >>= f = \x -> f (mx x) x

--7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
  -- fmap :: Expr (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var (f x)
  fmap f (Val i) = Val i
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
  pure = Var
  -- ()<*>) :: Expr (a -> b) -> Expr a -> Expr b
  _ <*> Val i = Val i
  Val i <*> _ = Val i
  Var f <*> Var x = Var (f x)
  Var f <*> Add x y = Add (fmap f x) (fmap f y)
  ---not sure about the other cases...
  ---Add x y <*> Var x = ?

instance Monad Expr where
  return = pure    -- ?

  -- (>>=) :: f a -> (a -> f b) -> f b
  Val x >>= _ = Val x
  Var x >>= f = f x
  Add x y >>= f = Add (x >>= f) (y >>= f)

--8
instance Functor ST where
  --fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do x <- st
                 S (\s -> (g x, s))

instance Applicative ST where
  --pure :: a -> ST a
  pure x = S (\s -> (x, s))

  --(<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do f <- stf
                   x <- stx
                   S (\s -> ((f x), s))

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s ->
     let (x,s') = app st s in app (f x) s')
