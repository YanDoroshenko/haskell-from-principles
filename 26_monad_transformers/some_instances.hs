module SomeInstances where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift .liftIO

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift .liftIO
