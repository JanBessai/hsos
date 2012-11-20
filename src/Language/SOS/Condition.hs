----------------------------------------------------------------
-- |
-- Module      :  Language.SOS.Condition
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines datatypes for conditions of rules.
----------------------------------------------------------------

module Language.SOS.Condition
  ( Predicate (..)
  , PathPredicate (..)
  , negateInnerPredicates
  ) where

-- | A CTL* State Predicate over values of type @t@.
data Predicate t
  = Top
  -- ^ Allways hold
  | Bottom
  -- ^ Never hold
  | Match t
  -- ^ Hold if the state at the current time matches t
  | Not (Predicate t)
  -- ^ Negate the predicate
  | And (Predicate t) (Predicate t)
  -- ^ Conjunction of two predicates
  | Or (Predicate t) (Predicate t)
  -- ^ Disjunction of two predicates
  | Implies (Predicate t) (Predicate t)
  -- ^ Implication of two predicates
  | Exists (PathPredicate t)
  -- ^ Path predicate holds on at least one branch
  | ForAll (PathPredicate t)
  -- ^ Path predicate holds on all branches
  deriving (Ord, Eq, Show)

-- | A CTL* Path Predicate over values of type @t@
data PathPredicate t
  = CurrentState (Predicate t)
  -- ^ Predicate holds at the current state
  | NotP (PathPredicate t)
  -- ^ Negation of a path predicate
  | AndP (PathPredicate t) (PathPredicate t)
  -- ^ Conjunction of two path predicates
  | OrP (PathPredicate t) (PathPredicate t)
  -- ^ Disjunction of two path predicates
  | ImpliesP (PathPredicate t) (PathPredicate t)
  -- ^ Implication of two path predicates
  | Next (PathPredicate t)
  -- ^ Path predicate holds at all direct successor states
  | Future (PathPredicate t)
  -- ^ Path predicate holds at some successor state
  | Globally (PathPredicate t)
  -- ^ Path predicate holds at all successor states
  | Until (PathPredicate t) (PathPredicate t)
  -- ^ First path predicate holds until the second becomes true
  deriving (Ord, Eq, Show)

-- | Negates the Predicates inside a PathPredicate
negateInnerPredicates :: PathPredicate t -> PathPredicate t
negateInnerPredicates (CurrentState p) = CurrentState $ Not p
negateInnerPredicates (NotP pp) = NotP $ negateInnerPredicates pp
negateInnerPredicates (AndP pp1 pp2) = AndP (negateInnerPredicates pp1) (negateInnerPredicates pp2)
negateInnerPredicates (OrP pp1 pp2) = OrP (negateInnerPredicates pp1) (negateInnerPredicates pp2)
negateInnerPredicates (ImpliesP pp1 pp2) = ImpliesP (negateInnerPredicates pp1) (negateInnerPredicates pp2)
negateInnerPredicates (Next pp) = Next $ negateInnerPredicates pp
negateInnerPredicates (Future pp) = Future $ negateInnerPredicates pp
negateInnerPredicates (Globally pp) = Globally $ negateInnerPredicates pp
negateInnerPredicates (Until pp1 pp2) = Until (negateInnerPredicates pp1) (negateInnerPredicates pp2)
