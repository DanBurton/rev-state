{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DoRec #-}

module Control.Monad.Trans.RevState
  ( -- * Monad Transformer
    RevStateT (RevStateT)
  , runStateT
  , evalStateT
  , execStateT
  , mapStateT
  , withStateT

    -- * Monad
  , RevState
  , runState
  , evalState
  , execState
  , mapState
  , withState

    -- * Primitives and basic combinators
  , get
  , put
  , state
  , gets  
  , modify
  ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Identity


newtype RevStateT s m a = RevStateT
  { runStateT :: s -> m (a, s) }

evalStateT :: Monad m => RevStateT s m a -> s -> m a
evalStateT m s = fst `liftM` runStateT m s

execStateT :: Monad m => RevStateT s m a -> s -> m s
execStateT m s = snd `liftM` runStateT m s

type RevState s = RevStateT s Identity

runState :: RevState s a -> s -> (a, s)
runState m s = runIdentity $ runStateT m s

evalState :: RevState s a -> s -> a
evalState m s = fst $ runState m s

execState :: RevState s a -> s -> s
execState m s = snd $ runState m s

instance MonadFix m => Monad (RevStateT s m) where
  return x = state $ \s -> (x, s)
  m >>= f = RevStateT $ \s -> do
    rec
      (x, s'') <- runStateT m s'
      (x', s') <- runStateT (f x) s
    return (x', s'')

instance MonadFix m => Applicative (RevStateT s m) where
  (<*>) = ap
  pure = return

instance Monad m => Functor (RevStateT s m) where
  -- this instance is hand-written
  -- so we don't have to rely on m being MonadFix
  fmap f m = RevStateT $ \s -> first f `liftM` runStateT m s

get :: Monad m => RevStateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> RevStateT s m ()
put s' = state $ \_s -> ((), s')

modify :: Monad m => (s -> s) -> RevStateT s m ()
modify f = state $ \s -> ((), f s)

state :: Monad m => (s -> (a, s)) -> RevStateT s m a
state f = RevStateT $ \s -> return (f s)


mapStateT :: (m (a, s) -> n (b, s)) -> RevStateT s m a -> RevStateT s n b
mapStateT f m = RevStateT $ f . runStateT m

withStateT :: (s -> s) -> RevStateT s m a -> RevStateT s m a
withStateT f m = RevStateT $ runStateT m . f

mapState :: ((a, s) -> (b, s)) -> RevState s a -> RevState s b
mapState f = mapStateT (Identity . f . runIdentity)

withState :: (s -> s) -> RevState s a -> RevState s a
withState = withStateT

gets :: Monad m => (s -> a) -> RevStateT s m a
gets f = fmap f get

