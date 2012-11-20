{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
----------------------------------------------------------------
-- |
-- Module      :  Test.Language.SOS.Data.Condition.Class.Test
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines tests for contitions on rules.
-- All meta properties (metaProp_*) are reusable with custom
-- monad instances.
----------------------------------------------------------------
module Language.SOS.Condition.Class.Test
  ( metaProp_sound
  , metaProp_interleaveWithResultCons
  , metaProp_fairSumIsMsum
  , runTests
  ) where

import           Control.Applicative
import           Control.Monad.Logic
import qualified Data.Set                     as Set
import           Test.QuickCheck
import           Test.QuickCheck.All

import           Language.SOS.Condition
import           Language.SOS.Condition.Class

-- | Generate arbitrary predicates
instance (Arbitrary t) => Arbitrary (Predicate t) where
  arbitrary =
    sized (\ n -> 
             let n' = min 4 (n-1) in
                 oneof . (if n < 2 then take 2 else id) $
                       [ return Top
                       , return Bottom
                       , Not <$> resize n' arbitrary
                       , And <$> resize n' arbitrary <*> resize n' arbitrary
                       , Or <$> resize n' arbitrary <*> resize n' arbitrary
                       , Implies <$> resize n' arbitrary <*> resize n' arbitrary
                       , Exists <$> resize n' arbitrary
                       , ForAll <$> resize n' arbitrary
                       ])

-- | Generate arbitrary path predicates
instance (Arbitrary t) => Arbitrary (PathPredicate t) where
  arbitrary =
    sized (\ n ->
             let n' = min 4 (n-1) in
                 oneof . (if n < 1 then take 1 else id) $
                       [ CurrentState <$> resize n' arbitrary
                       , NotP <$> resize n' arbitrary
                       , AndP <$> resize n' arbitrary <*> resize n' arbitrary
                       , OrP <$> resize n' arbitrary <*> resize n' arbitrary
                       , ImpliesP <$> resize n' arbitrary <*> resize n' arbitrary
                       , Next <$> resize n' arbitrary
                       , Future <$> resize n' arbitrary
                       , Globally <$> resize n' arbitrary
                       , Until <$> resize n' arbitrary <*> resize n' arbitrary
                       ])


-- | Monadic version of unfoldr
unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f x = do
  res <- f x
  case res of
    Nothing -> return []
    Just (r, x') -> do
                 rs <- unfoldrM f x'
                 return (r:rs)

-- | Turn a list of observations into a set
observationSet :: (Functor m, Monad m, MonadLogic m, Ord t) => m t -> m (Set.Set t)
observationSet xs = Set.fromList <$> unfoldrM msplit xs

-- | Check a logic instance to be sound.
metaProp_sound :: ( Applicative m
                  , Monad m
                  , MonadLogic m
                  , Ord t
                  , CheckPredicate t m
                  , Eq (m (Set.Set t))
                  )
               => (t -> m t)
               -- ^ Function to resolve variables
               -> (t -> [t])
               -- ^ Successor relation
               -> Predicate t
               -- ^ First predicate for connectors
               -> Predicate t
               -- ^ Second predicate for connectors
               -> PathPredicate t
               -- ^ First predicate for path connectors
               -> PathPredicate t
               -- ^ Second predicate for path connectors
               -> t
               -- ^ Term to check soundness on
               -> Bool
metaProp_sound resolved getNext p1 p2 pp1 pp2 t =
  let ss1 = observationSet $ checkPredicate p1 t
      ss2 = observationSet $ checkPredicate p2 t
      ss1' = observationSet $ checkPredicate (Exists pp1) t
      ss2' = observationSet $ checkPredicate (Exists pp2) t
      ts = observationSet $ resolved t
      es = observationSet $ mzero
  in observationSet (checkPredicate Top t) == ts
     && observationSet (checkPredicate Bottom t) == es
     && observationSet (checkPredicate (Match t) t) == ts
     && observationSet (checkPredicate (Not p1) t)
          == (Set.difference <$> ts <*> ss1)
     && observationSet (checkPredicate (And p1 p2) t)
          == (Set.intersection <$> ss1 <*> ss2)
     && observationSet (checkPredicate (Or p1 p2) t)
          == (Set.union <$> ss1 <*> ss2)
     && observationSet (checkPredicate (Implies p1 p2) t)
          == (if ss1 == es then ts else ss2)
     && observationSet (checkPredicate (Exists (CurrentState p1)) t)
          == ss1
     && observationSet (checkPredicate (Exists (NotP pp1)) t)
          == (Set.difference <$> ts <*> ss1')
     && observationSet (checkPredicate (Exists (AndP pp1 pp2)) t)
          == (Set.intersection <$> ss1' <*> ss2')
     && observationSet (checkPredicate (Exists (OrP pp1 pp2)) t)
          == (Set.union <$> ss1' <*> ss2')
     && observationSet (checkPredicate (Exists (ImpliesP pp1 pp2)) t)
          == (if ss1' == es then ts else ss2')
     && observationSet (checkPredicate (Exists (Next pp1)) t)
          == nextSet resolved getNext pp1 t
     {-&& observationSet (checkPredicate (Exists (Future pp1)) t)
          == futureSet resolved getNext pp1 t
     && observationSet (checkPredicate (Exists (Globally pp1)) t)
          == globallySet resolved getNext pp1 t
     && observationSet (checkPredicate (Exists (Until pp1 pp2)) t)
          == untilSet resolved getNext pp1 pp2 t
     && observationSet (checkPredicate (ForAll pp1) t)
          == (Set.difference
             <$> ts
             <*> (observationSet
                  $ checkPredicate (Not $ Exists (negateInnerPredicates pp1)) t))-}

-- | Find the set of next states where pp1
nextSet :: ( Functor m
           , Applicative m
           , Monad m
           , MonadLogic m
           , Ord t
           , CheckPredicate t m
           )
        => (t -> m t)
        -- ^ Function to resolve variables
        -> (t -> [t])
        -- ^ Successor relation
        -> PathPredicate t
        -- ^ Predicate that has to hold in future
        -> t
        -- ^ Term to check
        -> m (Set.Set t)
nextSet resolved getNext pp1 t = do
  t' <- resolved t
  s  <- Set.map (const t')
       <$> unionsM (map (denormalizeM
                         . observationSet 
                         . checkPredicate (Exists pp1))
                        (getNext t'))
  normalizeM s

-- | Fixpoint algorithm to build the set for Future pp1.
futureSet :: ( Functor m
             , Applicative m
             , Monad m
             , MonadLogic m
             , Ord t
             , CheckPredicate t m
             )
          => (t -> m t)
          -- ^ Function to resolve variables
          -> (t -> [t])
          -- ^ Successor relation
          -> PathPredicate t
          -- ^ Predicate that has to hold in future
          -> t
          -- ^ Term to check
          -> m (Set.Set t)
futureSet resolved getNext pp1 t = do
  t' <- resolved t
  s  <- Set.map (const t')
       <$> (Set.union
                 <$> unionsM (map (denormalizeM 
                                   . observationSet . checkPredicate (Exists pp1))
                                  (getNext t'))
                 <*> unionsM (map (denormalizeM
                                  . futureSet resolved getNext pp1) 
                                  (getNext t')))
  normalizeM s

-- | Fixpoint algorithm to build the set for Globally pp1.
globallySet :: ( Functor m
               , Applicative m
               , Monad m
               , MonadLogic m
               , Ord t
               , CheckPredicate t m
               )
            => (t -> m t)
            -- ^ Function to resolve variables
            -> (t -> [t])
            -- ^ Successor relation
            -> PathPredicate t
            -- ^ Predicate that has to hold globally
            -> t
            -- ^ Term to check
            -> m (Set.Set t)
globallySet resolved getNext pp1 t = do
  t' <- resolved t
  Set.map (const t')
       <$> (Set.intersection
                 <$> intersectionsM
                       (map (observationSet . checkPredicate (Exists pp1))
                            (getNext t'))
                 <*> intersectionsM (map (futureSet resolved getNext pp1) 
                                         (getNext t')))

-- | Fixpoint algorithm to build the set for Until pp1 pp2.
untilSet :: (Functor m
            , Applicative m
            , Monad m
            , MonadLogic m
            , Ord t
            , CheckPredicate t m
            )
         => (t -> m t)
         -- ^ Function to resolve variables
         -> (t -> [t])
         -- ^ Successor relation
         -> PathPredicate t
         -- ^ Predicate that has to hold until released
         -> PathPredicate t
         -- ^ Predicate which releases first predicate
         -> t
         -- ^ Term to check
         -> m (Set.Set t)
untilSet resolved getNext pp1 pp2 t = do
  t' <- resolved t
  s <- Set.union
       <$> (denormalizeM . observationSet $ checkPredicate (Exists pp2) t')
       <*> (Set.intersection
              <$> (denormalizeM
                   . observationSet $ checkPredicate (Exists pp1) t')
              <*> (Set.map (const t')
                   <$> unionsM (map (denormalizeM 
                                     . untilSet resolved getNext pp1 pp2)
                                    (getNext t'))))
  normalizeM s

-- | Normalize empty result sets to mzero
normalizeM :: MonadPlus m => (Set.Set t) -> m (Set.Set t)
normalizeM s = do
  guard (not $ Set.null s)
  return s

-- | Denormalize failures to an empty result set
denormalizeM :: MonadLogic m => m (Set.Set t) -> m (Set.Set t)
denormalizeM s = ifte s return (return Set.empty)

-- | Left associative monadic union over a list of sets
unionsM :: (Applicative m, MonadPlus m, Monad m, Ord t) 
        => [m (Set.Set t)] 
        -> m (Set.Set t)
unionsM = foldl (\ s mt -> Set.union <$> s <*> mt) (return Set.empty)

-- | Left associative monadic intersection over a list of sets
intersectionsM :: (Applicative m, Monad m, Ord t)
               => [m (Set.Set t)]
               -> m (Set.Set t)
intersectionsM [] = return (Set.empty)
intersectionsM (x:xs) = foldl (\ s mt -> Set.intersection <$> s <*> mt) x xs

-- | Check that interleaveWithResult is observationally equal to Cons.
metaProp_interleaveWithResultCons :: ( Monad m
                                     , MonadLogic m
                                     , Functor m
                                     , Eq (m [t])
                                     )
                                  => m t
                                  -> t
                                  -> Bool
metaProp_interleaveWithResultCons xs x =
  unfoldrM msplit (interleaveWithResult xs x) == ((x:) <$> unfoldrM msplit xs)

-- | Test 'metaProp_interleaveWithResult' on lists.
prop_interleaveWithResultCons :: (Eq t) => [t] -> t -> Bool
prop_interleaveWithResultCons xs =
  metaProp_interleaveWithResultCons xs

-- | Check that the fair sum contains only entries of msum
metaProp_fairSumIsMsum :: ( Monad m
                          , MonadLogic m
                          , Functor m
                          , Eq (m (Set.Set a))
                          , Ord a
                          )
                       => [m a]
                       -> Bool
metaProp_fairSumIsMsum xs =
  (observationSet $ msum xs) == (observationSet $ fairSum xs)

-- | Test 'metaProp_fairSumIsMsum' on lists.
-- Crop all lists to a maximum of 5 elements in order to reduce runtime.
prop_fairSumIsMsum :: (Ord t) => [[t]] -> Bool
prop_fairSumIsMsum xs =
  metaProp_fairSumIsMsum (map (take 5) $ take 5 xs)

-- | Check that 'runAndInterleave' behaves like monadic
-- sequencing and 'mplus'.
metaProp_runAndInterleaveMonadic :: ( Functor m
                                    , Monad m
                                    , MonadLogic m
                                    , Ord a
                                    , Eq (m (Set.Set a))                                    
                                    ) 
                                 => m a 
                                 -> m a 
                                 -> Bool
metaProp_runAndInterleaveMonadic m1 m2 =
  (observationSet $ monadicRAI m1 m2) 
  == (observationSet $ runAndInterleave m1 m2)
  where
    monadicRAI x y = do
      x' <- x
      y' <- y
      (return x') `mplus` (return y')

-- | Test 'metaProp_runAndInterleaveMonadic' on lists.
-- Crop all lists to a maximum of 5 elements in order to reduce runtime.
prop_runAndInterleaveMonadic :: (Ord t) => [t] -> [t] -> Bool
prop_runAndInterleaveMonadic m1 m2 =
  metaProp_runAndInterleaveMonadic (take 5 m1) (take 5 m2)

-- | Check that 'fairSequence' behaves like sequence.
metaProp_fairSequenceIsSequence :: ( Functor m
                                   , Monad m
                                   , MonadLogic m
                                   , Ord a
                                   , Eq (m (Set.Set a))
                                   )
                                => [m a]
                                -> Bool
metaProp_fairSequenceIsSequence xs =
  (Set.fromList . concat <$> unfoldrM msplit (sequence xs)) 
  == (observationSet $ fairSequence xs)
 
-- | Teste 'metaProp_fairSequenceIsSequence' on lists.
-- Crop all lists to a maximum of 3 elements in order to reduce runtime.
prop_fairSequenceIsSequence :: (Ord t) => [[t]] -> Bool
prop_fairSequenceIsSequence =
  metaProp_fairSequenceIsSequence . take 3 . map (take 3)

-- | Complete test suite for all properties of this module
runTests :: IO Bool
runTests = $quickCheckAll
