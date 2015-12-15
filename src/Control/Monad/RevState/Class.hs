{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DoRec #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.RevState.Class
  ( MonadRevState (..)
  , modify
  , gets
  ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Fix (MonadFix)

import qualified Control.Monad.Trans.RevState as Rev


class (Applicative m, MonadFix m) => MonadRevState s m | m -> s where
  get :: m s
  get = state $ \s -> (s, s)

  put :: s -> m ()
  put s = state $ \_ -> ((), s)

  state :: (s -> (a, s)) -> m a
  state f = do
    rec
      let ~(a, s') = f s
      put s'
      s <- get
    return a


modify :: MonadRevState s m => (s -> s) -> m ()
modify f = state $ \s -> ((), f s)

gets :: MonadRevState s m => (s -> a) -> m a
gets f = f `liftM` get


instance MonadFix m => MonadRevState s (Rev.StateT s m) where
  get = Rev.get
  put = Rev.put
  state = Rev.state
