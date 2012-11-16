{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
----------------------------------------------------------------
-- |
-- Module      :  Language.SOS.Data.Rule
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines datatypes for terms and patterns over
-- terms used in structural operational semantic rules.
----------------------------------------------------------------

module Language.SOS.Data.Term
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
import           Language.SOS.Data.Condition
import           Language.SOS.Data.Condition.Class
import           Prelude                           hiding (foldl)

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

instance (Monoid i) => CheckPredicate (Pattern i) (LogicT (IntBindingT (TermSpine i) Logic)) where
  checkPredicate (Match (Pattern pat)) (Pattern term) =
    (lift . runErrorT $ unify term pat) >>= either (const mzero) (return . Pattern)
  checkPredicate p (Pattern (UVar v)) = do
    t <- lift (lookupVar v)
    case t of
      Just term -> checkPredicate p (Pattern term)
      Nothing -> mzero
  checkPredicate (And p1 p2) term = (checkPredicate p1 term) >>- checkPredicate p2
  checkPredicate (Or p1 p2) term = interleave (checkPredicate p1 term) (checkPredicate p2 term)
  checkPredicate (Not p) term = ifte (checkPredicate p term) (const mzero) (return term)
  checkPredicate (Next p) (Pattern (UTerm (Constructor _ terms _))) =
    foldl (\ s term -> interleave s (checkPredicate p (Pattern term))) mzero terms
  checkPredicate (Future p) term@(Pattern (UTerm (Constructor _ terms _))) =
    interleave (checkPredicate (Next p) term)
               (foldl (\ s term' -> interleave s (checkPredicate (Future p) (Pattern term'))) mzero terms)
  checkPredicate (Globally p) term@(Pattern (UTerm (Constructor _ terms _))) =
    (foldl (\ s term' -> s >>- (\ r1 -> checkPredicate p (Pattern term') >>- (interleave (return r1) . return))) (return term) terms)
    >>- (\ s' -> foldl (\ s term' -> s >>- (\ r1 -> checkPredicate (Globally p) (Pattern term') >>- (interleave (return r1) . return))) (return s') terms)
  checkPredicate (ForAll p) term =
    checkPredicate p term >>- (\ r -> checkPredicate (Globally p) term >>- (interleave (return r) . return))
  checkPredicate (Exists p) term =
    interleave (checkPredicate p term) (checkPredicate (Future p) term)
  checkPredicate (Until p1 p2) term@(Pattern (UTerm (Constructor _ terms _))) =
    ifte (checkPredicate p1 term) (\ s' -> (foldl (\ s term' -> s >>- (\ r1 -> checkPredicate (Until p1 p2) (Pattern term') >>- (interleave (return r1) . return))) mzero terms) >>- (interleave (return s') . return)) (checkPredicate p2 term)
  checkPredicate (Release p1 p2) term =
    checkPredicate (Or (ForAll p2) (Exists p1
