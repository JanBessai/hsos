{-# LANGUAGE FunctionalDependencies #-}
----------------------------------------------------------------
-- |
-- Module      :  Language.SOS.Condition.Class
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines operations on conditions of rules.
----------------------------------------------------------------

module Language.SOS.Condition.Class
       ( CheckPredicate (..)
       , interleaveWithResult
       , fairSum
       , runAndInterleave
       , fairSequence
       ) where

import           Control.Monad.Logic
import           Language.SOS.Condition

class (MonadLogic m) => CheckPredicate t m | t -> m where
  checkPredicate :: Predicate t -> t -> m t

-- | Take a computation and interleave it with the
-- result of a previous computation
interleaveWithResult :: (MonadLogic m) => m t -> t -> m t
interleaveWithResult c res = interleave (return res) c

-- | Fairly interleave many computations.
fairSum :: (MonadLogic m) => [m t] -> m t
fairSum = foldr (interleave) mzero

-- | Fairly run both computations and interleave
-- their results if both succeed
runAndInterleave :: MonadLogic m => m t -> m t -> m t
runAndInterleave m1 m2 =
      m1 >>- (\ r1 -> m2 >>- (interleave (return r1) . return))

-- | Fairly conjunct many computations interleaving their results.
fairSequence :: (MonadLogic m) => [m t] -> m t
fairSequence [] = mzero
fairSequence xs = foldl1 runAndInterleave xs


