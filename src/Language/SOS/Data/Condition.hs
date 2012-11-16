----------------------------------------------------------------
-- |
-- Module      :  Language.SOS.Data.Rule
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines datatypes for conditions of rules.
----------------------------------------------------------------

module Language.SOS.Data.Condition
  ( Predicate (..)
  ) where

-- | A Predicate over values of type @t@.
data Predicate t
  = And (Predicate t) (Predicate t)
  -- ^ Conjunction of two predicates
  | Or (Predicate t) (Predicate t)
  -- ^ Disjunction of two predicates
  | Not (Predicate t)
  -- ^ Negation of a predicate
  | Match t
  -- ^ Match with a value
  | Next (Predicate t)
  -- ^ Evaluate the predicate on the next state of the value
  | Future (Predicate t)
  -- ^ Evaluate the predicate on some future state of the value
  | Globally (Predicate t)
  -- ^ Evaluate the prediate on all future states of the value
  -- and combine them using conjunction.
  | ForAll (Predicate t)
  -- ^ Evaluate the predicate on all states of the value
  -- including the current state and combine the results
  -- using.
  | Exists (Predicate t)
  -- ^ Evaluate the predicate on some state of the value
  | Until (Predicate t) (Predicate t)
  -- ^ Check if the second prediate holds until the first
  -- predicate evaluates to True
  | Release (Predicate t) (Predicate t)
  -- ^ Check if the first predicate holds until the second
  -- becomes true.
  deriving (Eq, Ord, Show)


