{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.RevState.Class
  ( MonadRevState (..)
  , modify
  , gets
  ) where

import Control.Applicative
import Control.Monad

class (Applicative m, Monad m) => MonadRevState s m | m -> s where
  get :: m s
  put :: s -> m ()
  state :: (s -> (a, s)) -> m a

modify :: MonadRevState s m => (s -> s) -> m ()
modify f = state $ \s -> ((), f s)

gets :: MonadRevState s m => (s -> a) -> m a
gets f = f `liftM` get
