{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
----------------------------------------------------------------
-- |
-- Module      :  Language.SOS.Data.Term
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines datatypes for terms and patterns over
-- terms used in structural operational semantic rules.
----------------------------------------------------------------

module Language.SOS.Term
  ( Pattern (..)
  , TermSpine (..)
  , Term
  , PatternVariable
  ) where

import           Control.Monad
import           Control.Monad.Logic
import           Control.Monad.Trans.Error
import           Control.Unification
import           Control.Unification.IntVar
import           Data.Foldable
import           Data.Functor.Fixedpoint
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Language.SOS.Condition
import           Language.SOS.Condition.Class
import           Prelude                      hiding (foldl)

-- | Patterns over ASTs which can be unified.
-- The 'Eq' and 'Ord' instances correspond to '=~=' and '<:='.
data Pattern i
  = Pattern (UTerm (TermSpine i) PatternVariable)
  deriving (Show)

-- | A fragment of an AST which is annotated by some info
-- of type 'i'. 'TermSpine's can be unified if their constructor
-- labels and their arity (number of elements in content) match.
data TermSpine i a =
  Constructor
  { label   :: String
  -- ^ Constructor
  , content :: [a]
  -- ^ Child nodes/fields
  , info    :: i
  } deriving (Eq, Functor, Foldable, Traversable, Show)

-- | The fixpoint over AST-Fragments yields a complete AST.
type Term i = Fix (TermSpine i)

-- | PatternVariables are 'IntVar' in order to allow
-- backtracking during unification and to enhance performance.
type PatternVariable = IntVar

instance (Monoid i) => Unifiable (TermSpine i) where
  zipMatch t1 t2 = do
    guard (label t1 == label t2)
    guard (length (content t1) == length (content t2))
    return $ Constructor (label t1)
                         (map Right $ zip (content t1) (content t2))
                         (info t1 `mappend` info t2)

instance (Monoid i) => Eq (Pattern i) where
  (Pattern p) == (Pattern p') =
    isJust
    . runIdentity
    . evalIntBindingT
    $ p =~= p'

instance (Monoid i) => Ord (Pattern i) where
  (Pattern p) <= (Pattern p') =
    either (const False) id
    . runIdentity
    . evalIntBindingT
    . runErrorT
    $ p <:= p'

checkPredicate' :: (Monoid i)
                => Predicate (Pattern i)
                -> Pattern i
                -> IntBindingT (TermSpine i) Logic (Pattern i)
checkPredicate' p (Pattern (UVar v)) = do
    t <- lookupVar v
    case t of
      Just term -> checkPredicate' p (Pattern term)
      Nothing -> mzero
checkPredicate' (Top) term =
  return term
checkPredicate' (Bottom) _ =
  mzero
checkPredicate' (Match (Pattern pat)) (Pattern term) = do
  unificationResult <- runErrorT $ unify term pat
  case unificationResult of
    Left _ -> mzero
    Right term' -> return (Pattern term')
checkPredicate' (Not p) term =
  ifte (checkPredicate' p term) (const mzero) (return term)
checkPredicate' (And p1 p2) term =
  runAndInterleave (checkPredicate' p1 term) (checkPredicate' p2 term)
checkPredicate' (Or p1 p2) term =
  checkPredicate' p1 term `interleave` checkPredicate' p2 term
checkPredicate' (Implies p1 p2) term =
  ifte (checkPredicate' p1 term)
       (const $ checkPredicate' p2 term >> return term)
       (return term)
checkPredicate' (Exists pp) term =
  checkPathPredicate pp term >> return term
checkPredicate' (ForAll pp) term =
  checkPredicate' (Not (Exists (negateInnerPredicates pp))) term

-- | Check a path predicate on a term
checkPathPredicate :: (Monoid i)
                   => PathPredicate (Pattern i)
                   -> Pattern i
                   -> IntBindingT (TermSpine i) Logic (Pattern i)
checkPathPredicate p (Pattern (UVar v)) = do
    t <- lookupVar v
    case t of
      Just term -> checkPathPredicate p (Pattern term)
      Nothing -> mzero
checkPathPredicate (CurrentState p) term =
  checkPredicate p term
checkPathPredicate (NotP pp) term =
  ifte (checkPathPredicate pp term) (const mzero) (return term)
checkPathPredicate (AndP pp1 pp2) term =
  runAndInterleave (checkPathPredicate pp1 term) (checkPathPredicate pp2 term)
checkPathPredicate (OrP pp1 pp2) term =
  checkPathPredicate pp1 term `interleave` checkPathPredicate pp2 term
checkPathPredicate (ImpliesP pp1 pp2) term =
  ifte (checkPathPredicate pp1 term)
       (const $ checkPathPredicate pp2 term >> return term)
       (return term)
checkPathPredicate (Next pp) (Pattern (UTerm (Constructor _ terms _))) =
  fairSum $ map (checkPathPredicate pp . Pattern) terms
checkPathPredicate pp@(Future pp') (Pattern (UTerm (Constructor _ terms _))) =
  fairSum $ map (checkPathPredicate (OrP pp' pp) . Pattern) terms
checkPathPredicate pp@(Globally pp') (Pattern (UTerm (Constructor _ terms _))) =
  fairSequence
  $ map
      (checkPathPredicate (AndP pp' (CurrentState $ ForAll pp)) . Pattern)
      terms
checkPathPredicate pp@(Until pp1 pp2) term =
  ifte (checkPathPredicate pp1 term)
       (const $ return term)
       (checkPathPredicate pp2 term
        >>- const (checkPathPredicate (Next pp) term)
        >>- const (return term))


instance (Monoid i) =>
  CheckPredicate (Pattern i) (IntBindingT (TermSpine i) Logic) where
  checkPredicate = checkPredicate'
