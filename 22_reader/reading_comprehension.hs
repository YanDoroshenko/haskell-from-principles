module ReadingComprehension where

newtype Reader r a = Reader { runReader :: r -> a }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap g (Reader f) = Reader $ g . f

instance Applicative (Reader r) where
  pure = Reader . const
  (Reader g) <*> (Reader f) = Reader $ g <*> f
