--1
instance (Monoid a, Monoid b) => Monoid (a,b) where
  --mempty :: (a,b)
  mempty = (mempty, mempty) -- why isn't it () ?

  --mappend :: (a,b) -> (a,b) -> (a,b)
  (x1, y1) 'mappend' (x2,y2) = (x1 'mappend' x2, y1 'mappend' y2)

--2
instance (Monoid a, Monoid b) => Monoid (a -> b) where
  --mempty :: (a -> b)
  mempty = \_ -> mempty

  --mappend :: (a -> b)  -> (a -> b)  -> (a -> b)
  a 'mappend' b = \x -> a x 'mappend' b x

--3
instance Foldable Maybe where
  --fold :: Monoid a => Maybe a -> a
  fold Nothing = mempty
  fold (Just x) = x
  --foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap f Nothing = mempty
  foldMap f (Just x) = f x
  --foldr :: (a -> b) -> b -> Maybe a -> b
  foldr f v Nothing = v
  foldr f v (Just x) = f x v
  --foldl :: (a -> b -> a) -> a -> Maybe b -> a
  foldl f v Nothing = v
  foldl f v (Just x) = f v x

instance Traversable Maybe where
  --traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse g Nothing = pure Nothing
  traverse g (Just x) = pure Just <*> (g x)

--4
instance Foldable Tree where
  --fold :: Monoid a => Tree a -> a
  fold Leaf = mempty
  fold (Node l x r) = fold l 'mappend' (x 'mappend' fold r)

  --foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap g Leaf = mempty
  foldMap g (Node l x r) = foldMap g l 'mappend' (g x) 'mappend' foldMap g r
  --foldr :: (a -> b) -> b -> Tree a -> b
  foldr g v Leaf = v
  foldr g v (Node l x r) = foldr g (g x (foldr g v r)) l
  --foldl :: (a -> b -> a) -> a -> Tree a -> a
  foldr g v Leaf = v
  foldr g v (Node l x r) = foldr g (g x (foldr g v l)) r

instance Traversable Tree where
  --traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf = pure Leaf
  traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r

--5
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF g x = [x | any . foldMap g x] --get back to this later, not sure whats going on 
