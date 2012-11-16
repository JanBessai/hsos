{-# LANGUAGE FunctionalDependencies #-}
----------------------------------------------------------------
-- |
-- Module      :  Language.SOS.Data.Rule
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines operations on conditions of rules.
----------------------------------------------------------------

module Language.SOS.Data.Condition.Class
       ( CheckPredicate (..)
       ) where

import           Control.Monad.Logic
import           Language.SOS.Data.Condition

class (MonadLogic m) => CheckPredicate t m | t -> m where
  checkPredicate :: Predicate t -> t -> m t
