----------------------------------------------------------------
-- |
-- Module      :  Language.SOS.Parser.Rule
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines how to parse operational semantics rules.
----------------------------------------------------------------

module Language.SOS.Parser.Rule where

import           Control.Applicative        hiding (empty, many, (<|>))
import           Control.Arrow              (first)
import           Control.Monad
import           Control.Unification
import           Control.Unification.IntVar
import           Data.Char
import           Data.Functor.Identity
import           Data.IntMap                (IntMap, fromList, union)
import           Data.Map                   (Map, assocs, empty, insert, lookup)
import           Data.Monoid
import           Prelude                    hiding (lookup)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Language
import           Text.Parsec.Token

import           Language.SOS.Data.Rule     hiding (pattern)

-- | A variation on 'haskellStyle'.
-- The following operators are reserved:
-- > [ "|-", "=>", "/", "&", "|", "~", "=", "N", "F", "G", "A", "E", "U", "R" ]
-- Rules for identifiers etc. are the same as for Haskell.
sosStyle :: GenLanguageDef String u Identity
sosStyle =
  haskellStyle
  { opStart  = opLetter sosStyle
  , opLetter = oneOf "=>?~/-|&NFGAEUR"
  , reservedOpNames = [ "|-", "=>", "/"
                      , "&", "|", "~", "="
                      , "N", "F", "G", "A", "E", "U", "R"
                      ]
  }

-- | A default parsec lexer for the sosStyle
lexer :: GenTokenParser String u Identity
lexer = makeTokenParser sosStyle

-- | Parse a 'Name' from a 'String' containing an identifier.
name :: ParsecT String u Identity Name
name = Name <$> identifier lexer

-- | Parse a constructor inside of a 'TermSpine' from an
-- upper-case identifier
constructor :: ParsecT String u Identity String
constructor = do
  Name ctor <- name
  if isUpper (head ctor)
     then return ctor
     else fail "AST identifiers start with an upper case letter."

-- | Mapping of identifiers to variables, including the next
-- free variable.
data IdentifierMapping
  = IdentifierMapping (Map Name PatternVariable) Int

-- | Update a 'IdentifierMapping' by inserting the new identifier
-- with a fresh 'PatternVariable' if it did not exist. Include
-- the 'PatternVariable' for the identifier in the result.
updateIdentifierMapping ::
  Name
  -> IdentifierMapping
  -> (IdentifierMapping, PatternVariable)
updateIdentifierMapping var im@(IdentifierMapping mapping nextFresh) =
  case lookup var mapping of
    Just pv -> (im, pv)
    Nothing -> ( IdentifierMapping (insert var (IntVar nextFresh) mapping)
                                 (nextFresh + 1)
               , IntVar nextFresh)

-- | Parse a variable from a lower-case identifier using the given
-- 'IdentifierMapping'.
variable ::
  IdentifierMapping
  -> ParsecT String u Identity (IdentifierMapping, PatternVariable)
variable im = do
  var@(Name n) <- name
  if not . isLower $ head n
    then fail "Variable identifiers start with a lower case letter."
    else return $ updateIdentifierMapping var im

-- | Mapping of variables to identifiers, including the next
-- free variable.
data VariableMapping
  = VariableMapping (IntMap Name) Int
  deriving (Show)

-- | Converts an 'IdentifierMapping' to a 'VariableMapping'
toVariableMapping :: IdentifierMapping -> VariableMapping
toVariableMapping (IdentifierMapping mapping nextFresh) =
  VariableMapping (fromList . flipAssocs. assocs $ mapping) nextFresh
  where
    flipAssocs = map (\ (vid, IntVar i) -> (i, vid))


-- | Parse a 'Pattern'. Patterns can be enclosed in parenthesis and contain
-- variableIdentifiers or constructors. Keep track of identifiers in
-- the current state.
pattern :: (Monoid i) => ParsecT String VariableMapping Identity (Pattern i)
pattern =
  Pattern
  <$> (try (parens lexer pattern') <|> pattern')
  <?> "pattern."
  where
    variable' = do
      (VariableMapping vm fresh) <- getState
      (im, var) <- variable (IdentifierMapping empty fresh)
      let (VariableMapping vm' fresh') = toVariableMapping im
      putState (VariableMapping (vm `union` vm') fresh')
      return $ UVar var
    constructor' = do
      ctor <- constructor
      children <- many pattern'
      return . UTerm $ Constructor ctor children mempty
    pattern' = try variable' <|> constructor'




{-
programState =
  ( ProgramState
    <$> variableIdentifier
    <*> (option [] $ braces lexer (commaSep lexer modification))
  )
  <?> "program state."

modification =
  ( Substitute
    <$> variableIdentifier
    <*> (reservedOp lexer "/" >> evaluation))
  <?> "substitution."

evaluation =
  try ( EvaluateVariable
        <$> (brackets lexer variableIdentifier)
        <*> (parens lexer programState)
      )
  <|> (EvaluateLiteral <$> stringLiteral lexer)
  <?> "variable evaluation or string literal."

stepPair = angles lexer $
           (,)
           <$> statement
           <*> (comma lexer >> programState)

configuration =
  try ( Left  <$> stepPair )
  <|> ( Right <$> programState )
  <?> "pair of statement and programstate or program state."

step =
  ( Step <$> configuration
         <*> (reservedOp lexer "=>" >> configuration) )
  <?> "calculation step."

condition =
  try ( In
        <$> variableIdentifier
        <*> (reservedOp lexer "in" >> programState)
      )
  <|> try ( Equals
            <$> evaluation
            <*> (reservedOp lexer "==" >> evaluation)
          )
  <|> (reservedOp lexer "~" >> parens lexer condition)
  <?> "condition."

premiss =
   (,) <$> name <*> (reservedOp lexer "|-" >> step)

rule =
  Rule
  <$> (braces lexer $ commaSep lexer premiss)
  <*> (braces lexer $ step)
  <*> (
       option [] $ reservedOp lexer "|"
                   >> commaSep1 lexer condition
      )

semantic =
  Semantic
  <$> name
  <*> (braces lexer $ rule `sepEndBy` semi lexer)

parseOperationalSemantic = do
  whiteSpace lexer
  sems <- many semantic
  eof
  return sems
-}
