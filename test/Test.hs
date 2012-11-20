----------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Copyright (c) 2012-- Jan Bessai
-- License     :  BSD
-- Maintainer  :  Jan.Bessai@tu-dortmund.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines a toplevel test suite for all
-- tests of hsos
----------------------------------------------------------------
module Main where

import qualified Language.SOS.Condition.Class.Test
import qualified Language.SOS.Term.Test

main = do
  Language.SOS.Condition.Class.Test.runTests
  Language.SOS.Term.Test.runTests
