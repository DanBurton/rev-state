{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.RevState
   ( module Control.Monad.RevState.Class
   , module Control.Monad.Trans.RevState
   ) where

import Control.Monad.RevState.Class
import Control.Monad.Trans.RevState hiding
  ( get
  , put
  , state
  , modify
  , gets
  )

import qualified Control.Monad.Trans.RevState as R
import Control.Monad.Fix

instance MonadFix m => MonadRevState s (RevStateT s m) where
  get = R.get
  put= R.put
  state = R.state
