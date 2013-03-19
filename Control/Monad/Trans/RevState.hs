{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DoRec #-}

module Control.Monad.Trans.RevState
  ( -- * Monad Transformer
    StateT (StateT)
  , runStateT
  , evalStateT
  , execStateT
  , mapStateT
  , withStateT

    -- * Monad
  , State
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


newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s) }

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = fst `liftM` runStateT m s

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = snd `liftM` runStateT m s

type State s = StateT s Identity

runState :: State s a -> s -> (a, s)
runState m s = runIdentity $ runStateT m s

evalState :: State s a -> s -> a
evalState m s = fst $ runState m s

execState :: State s a -> s -> s
execState m s = snd $ runState m s

instance MonadFix m => Monad (StateT s m) where
  return x = state $ \s -> (x, s)
  m >>= f = StateT $ \s -> do
    rec
      (x, s'') <- runStateT m s'
      (x', s') <- runStateT (f x) s
    return (x', s'')

instance MonadFix m => Applicative (StateT s m) where
  (<*>) = ap
  pure = return

instance Monad m => Functor (StateT s m) where
  -- this instance is hand-written
  -- so we don't have to rely on m being MonadFix
  fmap f m = StateT $ \s -> first f `liftM` runStateT m s


instance MonadFix m => MonadFix (StateT s m) where
  mfix f = StateT $ \s ->
    mfix (\(x, _) -> runStateT (f x) s)


get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s' = state $ \_s -> ((), s')

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT $ \s -> return (f s)


mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f = mapStateT (Identity . f . runIdentity)

withState :: (s -> s) -> State s a -> State s a
withState = withStateT

gets :: Monad m => (s -> a) -> StateT s m a
gets f = fmap f get

