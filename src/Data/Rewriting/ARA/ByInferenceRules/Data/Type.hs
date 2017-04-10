{-# LANGUAGE TemplateHaskell #-}
-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed May  4 10:38:10 2016 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Dec 20 23:57:39 2016 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 7
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
--
--

-- Code:

module Data.Rewriting.ARA.ByInferenceRules.Data.Type where

import           Control.Lens

data Data a = Data
              { _lab :: String
              , _val :: a
              } deriving (Show)
makeLenses ''Data

--
-- Type.hs ends here
