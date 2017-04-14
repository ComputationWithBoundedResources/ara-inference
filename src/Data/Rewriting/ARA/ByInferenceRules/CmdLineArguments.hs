-- CommandLineArguments.hs ---
--
-- Filename: CommandLineArguments.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Sep  4 12:25:37 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Apr 11 14:34:02 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 38
-- URL:
-- Doc URL:
-- Keywords:
-- Compatibility:
--
--

-- Commentary:
--
--
--
--

-- Change Log:
--
--
--

--
--

-- Code:

-- | Reexporting the modules in ./CmdLineArguments
module Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments
    (
     -- reexported modules
      module Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments.Type
    , module Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments.Parse
    ) where

import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments.Parse
import           Data.Rewriting.ARA.ByInferenceRules.CmdLineArguments.Type


--
-- CmdLineArguments.hs ends here
