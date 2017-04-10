-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Oct  1 15:39:56 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Dec 20 21:55:36 2016 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 99
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

-- | TODO: comment this module
module Data.Rewriting.ARA.ByInferenceRules.AnalyzerCost.Type
    ( ACost (..)
    , getACostValue
    )
    where


import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.Exception

import           Control.Exception                            (throw)


newtype ACost a = ACost a
  deriving (Show, Eq, Ord)

getACostValue :: ACost a -> a
getACostValue (ACost x) = x

instance Num a => Num (ACost a) where
  (ACost x) + (ACost y) = ACost (x+y)
  (ACost x) - (ACost y) = ACost (x-y)
  (ACost x) * (ACost y) = ACost (x*y)
  abs (ACost x) = ACost (abs x)
  signum (ACost x) = ACost (signum x)
  fromInteger x = ACost (fromInteger x)

--
-- Type.hs ends here


