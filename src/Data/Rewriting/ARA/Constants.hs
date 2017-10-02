-- Constants.hs ---
--
-- Filename: Constants.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Sep  4 10:46:58 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Mon Oct  2 11:17:48 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 100
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

module Data.Rewriting.ARA.Constants where

import           Text.PrettyPrint


-- Constant Doc elements
seperatorDoc :: Doc
seperatorDoc = text "-------------------------------------------------------------------------"


-- Variable
varPrefix :: String
varPrefix = "ipvar"

rhsBtmSym :: String
rhsBtmSym = "btm"

--
-- Constants.hs ends here
