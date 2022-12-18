module StateT where

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT mas) = StateT $ \s -> fmap (\(a, s') -> (f a, s')) $ mas s

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> (,s) <$> pure a
  (StateT fs) <*> (StateT fa) = StateT $ \s -> do
    (f, s') <- fs s
    (a, s'') <- fa s'
    return (f a, s'')
