module EitherT where

newtype EitherT e m a =  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ Right <$> pure a
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance Monad m => Monad (EitherT e m) where
  (EitherT mea) >>= f = EitherT $ do
    ea <- mea
    case ea of
      Right a -> runEitherT $ f a
      Left e -> return $ Left e

swapEither :: Either l r -> Either r l
swapEither (Right r) = Left r
swapEither (Left l) = Right l

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ (fmap swapEither) mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mea) = do
  ea <- mea
  case ea of
    Left e -> f e
    Right a -> g a
