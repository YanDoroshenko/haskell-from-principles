module WriteStateForYourself where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s ->
    let (a, s') = g s
        b = f a
     in (b, s')

instance Applicative (Moi s) where
  pure = Moi <$> (,)
  (Moi g) <*> (Moi f) = Moi $ \s ->
    let (a, s') = f s
        (f', s'') = g s
        b = f' a
    in (b, s'')

instance Monad (Moi s) where
  (Moi f) >>= g = Moi $ \s ->
    let (a, s') = f s
        (b, s'') = runMoi (g a) s
    in (b, s'')
