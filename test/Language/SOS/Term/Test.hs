{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
----------------------------------------------------------------
-- |
-- Module      :  Language.SOS.Data.Term.Test
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines tests for opreations on terms.
----------------------------------------------------------------

module Language.SOS.Term.Test
  ( runTests
  ) where

import           Control.Applicative
import           Control.Monad.Logic
import           Control.Unification
import           Control.Unification.IntVar
import           Data.Monoid
import qualified Data.Set                          as Set
import           Test.QuickCheck
import           Test.QuickCheck.All

import           Language.SOS.Condition
import           Language.SOS.Condition.Class.Test hiding (runTests)
import           Language.SOS.Term

instance Eq (IntBindingT (TermSpine ()) Logic (Set.Set (Pattern ()))) where
  x == y = (map fst . observeAll $ runIntBindingT x)
           == (map fst . observeAll $ runIntBindingT y)

instance Arbitrary (UTerm (TermSpine ()) IntVar) where
  arbitrary = sized (\ n ->
                       let n' = min 5 (n-1) in
                       oneof . (if n < 1 then take 1 else id) $
                             [ UVar . IntVar <$> arbitrary
                             , UTerm
                               <$> (Constructor <$> arbitrary
                                                <*> resize (n') arbitrary
                                                <*> arbitrary)
                             ])

instance Arbitrary (Pattern ()) where
  arbitrary = Pattern <$> arbitrary

-- | Tests if CTL* logic on patterns is sound.
prop_sound :: Predicate (Pattern ())
           -> Predicate (Pattern ())
           -> PathPredicate (Pattern ())
           -> PathPredicate (Pattern ())
           -> Pattern ()
           -> Bool
prop_sound p1 p2 pp1 pp2 pat =
  metaProp_sound resolved nextRel p1 p2 pp1 pp2 pat
  where
    nextRel (Pattern (UTerm (Constructor _ xs _))) = map Pattern xs
    nextRel _ = []
    resolved (Pattern (UVar v)) =
      lookupVar v >>= maybe mzero (return . Pattern)
    resolved p = return p

runTests :: IO Bool
runTests = $quickCheckAll
