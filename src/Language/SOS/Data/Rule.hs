module Language.SOS.Data.Rule where

newtype Rule = 
  Rule Name [Premiss] Conclusion [CompileTimeCondition] [RuntimeCondidtion]
  deriving (Eq, Ord, Show)

type Name = String

type Premiss = Transformation 
type Conclusion = Transformation

newtype CompileTimeCondition = CTC
newtype RuntimeCondition = RTC

