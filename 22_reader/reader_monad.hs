module ReaderMonad where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap g (Reader f) = Reader $ g . f

instance Applicative (Reader r) where
  pure = Reader . const
  (Reader g) <*> (Reader f) = Reader $ g <*> f

instance Monad (Reader r) where
  (Reader f) >>= g = Reader $ \r -> runReader (g $ f r) r
