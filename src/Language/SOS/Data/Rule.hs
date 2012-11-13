{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
----------------------------------------------------------------
-- |
-- Module      :  Language.SOS.Data.Rule
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines datatypes for rules used in structural
-- operational semantics specifications.
----------------------------------------------------------------

module Language.SOS.Data.Rule
  ( Rule (..)
  , Name (..)
  , Premiss
  , Conclusion
  , BooleanLogic (..)
  , Transformation (..)
  , RuleContext (..)
  , SemanticTriple (..)
  , Pattern (..)
  , TermSpine (..)
  , Term
  , PatternVariable
  , Context
  , ProgramState (..)
  , Modification (..)
  ) where

import           Control.Monad
import           Control.Monad.Trans.Error
import           Control.Unification
import           Control.Unification.IntVar
import           Data.Foldable
import           Data.Functor.Fixedpoint
import           Data.Functor.Identity
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable

-- | The type of SOS-Rules. The variable @i@ is the type of additional
-- information about things like sourcecode locations for error
-- repoting. It is usually expected to be an instance of 'Monoid' to
-- be able to combine it during unification.
data Rule i
  = Rule
  { ruleName             :: Name -- ^ Identifier
  , premisses            :: [Premiss i] -- ^ List of conjuncted premisses
  , conclusion           :: Conclusion i -- ^ Conclusion
  , compileTimeCondition :: BooleanLogic (Pattern i)
  -- ^ Guard evaluated during compile / interpretation time
  , runtimeCondition     :: BooleanLogic (ProgramState i)
  -- ^ Guard evaluated during runtime
  , ruleInfo             :: i
  -- ^ Additional information about this rule
  } deriving (Show)

-- | Identifiers of 'RuleContext', 'Rule' and 'Modification'.
newtype Name
  = Name String
  deriving (Eq, Ord, Show)

type Premiss = Transformation
type Conclusion = Transformation

-- | A transformation relates 'SemanticTriple's to each other
-- and belongs to a 'RuleContext' (i.e. namespace).
data Transformation i
  = Transformation
  { ruleContext :: Name
  -- ^ Identifier of the context the transformation belongs to
  , source      :: SemanticTriple i
  -- ^ Left hand side of the relation
  , target      :: SemanticTriple i
  -- ^ Right hand side of the relation
  } deriving (Show)

-- | A full description of the current program state.
data SemanticTriple i
  = SemanticTriple
  { context :: Context i
  -- ^ Context including all visible compiletime defined symbols
  , pattern :: Pattern i
  -- ^ AST-Pattern for the sourcecode which is currently run
  , state   :: ProgramState i
  -- ^ State of the machine model the program runs on
  } deriving (Show)

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

-- | The context of currently visible compiletime defined symbols.
type Context i = Map (Pattern i) (Term i)

-- | Program states have names and enlist a series
-- of virtual machine state modifications.
data ProgramState i
  = ProgramState Name [Modification i]
  deriving (Eq, Show)

-- | Modification of the state of a virtual machine.
-- This is done by substituting some named state attribute
-- to the (potentially big-step) evaluation of a Term.
data Modification i
  = Substitute Name (Term i)
  deriving (Eq, Show)


-- | A Logic Operation over values of type @t@  which
-- result in a boolean value. If the underlying values
-- support it these may even include temporal logic.
data BooleanLogic t
  = And (BooleanLogic t) (BooleanLogic t)
  -- ^ Conjunction of two operations
  | Or (BooleanLogic t) (BooleanLogic t)
  -- ^ Disjunction of two operations
  | Not (BooleanLogic t)
  -- ^ Negation of an operation
  | Eq t t
  -- ^ Equate two values
  | Next t (BooleanLogic t)
  -- ^ Evaluate the operation on the next state of the value
  | Future t (BooleanLogic t)
  -- ^ Evaluate the operation on some future state of the value
  | Globally t (BooleanLogic t)
  -- ^ Evaluate the operation on all future states of the value
  -- and combine them using conjunction.
  | All t (BooleanLogic t)
  -- ^ Evaluate the operation on all states of the value
  -- including the current state and combine the results
  -- using and.
  | Exists t (BooleanLogic t)
  -- ^ Evaluate the operation on some state of the value
  | Until t (BooleanLogic t) (BooleanLogic t)
  -- ^ Check if the second operation holds until the first
  -- operation evaluates to True
  | Release t (BooleanLogic t) (BooleanLogic t)
  -- ^ Check if the first operation holds until the second
  -- becomes true.
  deriving (Eq, Ord, Show)

-- | Context of Rules (i.e. namespace / named relation).
data RuleContext i
  = RuleContext Name [Rule i]
  deriving (Show)

-- | Annotation type for Terms and Rules.
data Annotation i ti
  = Annotation String i
  -- ^ Rules are annotated with a user supplied 'String' and infos @i@ about source positions.
  | TermAnnotation ti
  -- ^ Terms are annotated with some term info @ti@.
  deriving (Ord, Eq, Show)


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
