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
-- operational semantic specifications.
----------------------------------------------------------------

module Language.SOS.Data.Rule
  ( Rule (..)
  , Name (..)
  , Premiss
  , Conclusion
  , Predicate (..)
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

import           Data.Map                    (Map)
import           Language.SOS.Data.Condition
import           Language.SOS.Data.Term

-- | The type of SOS-Rules. The variable @i@ is the type of additional
-- information about things like sourcecode locations for error
-- repoting. It is usually expected to be an instance of 'Monoid' to
-- be able to combine it during unification.
data Rule i
  = Rule
  { ruleName             :: Name -- ^ Identifier
  , premisses            :: [Premiss i] -- ^ List of conjuncted premisses
  , conclusion           :: Conclusion i -- ^ Conclusion
  , compileTimeCondition :: Predicate (Pattern i)
  -- ^ Guard evaluated during compile / interpretation time
  , runtimeCondition     :: Predicate (ProgramState i)
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



