module LibraryFunctions where

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\l r -> r || (l == x)) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr f Nothing
  where
    f l r = maybe (Just l) (Just <$> (min l)) r

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing
  where
    f l r = maybe (Just l) (Just <$> (max l)) r

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: Foldable t => t a -> Int
length = foldr (\_ r -> r + 1) 0

toList :: Foldable t => t a -> [a]
toList = foldMap pure

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (\l r -> f l <> r) mempty
