{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun May 22 19:09:57 2016 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Apr  2 19:11:14 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 84
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

module Data.Rewriting.ARA.ByInferenceRules.ConstraintSolver.SMT.Type where

import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerCondition.Type
import           Data.Rewriting.ARA.ByInferenceRules.AnalyzerDatatype.Type
import           Data.Rewriting.ARA.ByInferenceRules.Data.Type
import           Data.Rewriting.ARA.ByInferenceRules.HelperFunctions
import           Data.Rewriting.ARA.ByInferenceRules.Operator
import           Data.Rewriting.ARA.ByInferenceRules.TypeSignatures
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Pretty
import           Data.Rewriting.ARA.ByInferenceRules.Vector.Type
import           Data.Rewriting.ARA.Exception
import           Data.Rewriting.Signature

import           Control.Arrow
import           Control.Exception                                          (throw)
import           Control.Lens
import           Data.List
import qualified Data.Map                                                   as M
import           Data.Maybe
import qualified Data.Set                                                   as S
import qualified Data.Text                                                  as T

data SMTProblem = SMTProblem
                  { _vars           :: S.Set T.Text -- [T.Text]
                  , _assertions     :: [(T.Text, Comparison, T.Text)]
                  , _assertionsStr  :: [T.Text]
                  , _ifs            :: [([(T.Text, T.Text)], [(T.Text,T.Text)])]
                  -- , _ifsComp        :: [([(T.Text, Comparison, T.Text)])]
                  , _values         :: M.Map T.Text Int
                  , _programName    :: T.Text
                  , _programOptions :: [T.Text]
                  , _parseFunction  :: T.Text -> ([Data Int], [Data Vector])
                  }
makeLenses ''SMTProblem

instance Show SMTProblem where
  show (SMTProblem vars ass assstr ifs vals n o _) =
    "Vars: " ++ show vars ++ "\nAssertions: "
    ++ show ass ++ "\nAssertion-T.Texts: " ++ show assstr ++
    "\nIf-Assertions: " ++ show ifs ++ "\nValues: " ++ show vals ++
    "\nProgram call: " ++ T.unpack n ++ " " ++ T.unpack (T.unwords o)


(+++) = T.append
infixl 5 +++


-- Conversion from/to SMT strings

replList' :: [(T.Text, T.Text)]
replList' = -- map (T.pack *** T.pack)
  replList
  where replList :: [(T.Text, T.Text)]
        replList = [ ("#", "_HASHTAG_")
                   , (":", "_COLON_")
                   , ("+", "_PLUS_")
                   , ("'", "_PRIME_")
                   , (";", "_SEMICOLON_")
                   , ("[", "_LBRA_")
                   , ("]", "_RBRA_")
                   , ("(", "_LPAREN_")
                   , (")", "_RPAREN_")

                   ]


convertToSMTText :: T.Text -> T.Text
convertToSMTText x = foldl replaceText x replList'
  where replaceText acc (from, to) = T.replace from to acc

convertToSMTStringText :: String -> T.Text
convertToSMTStringText x = foldl replaceText (T.pack x) replList'
  where replaceText acc (from, to) = T.replace from to acc


convertToSMTString :: String -> String
convertToSMTString x = T.unpack $ foldl replaceText (T.pack x) replList'
  where replaceText acc (from, to) = T.replace from to acc

convertFromSMTText :: T.Text -> T.Text
convertFromSMTText x = foldl replaceText x replList'
  where replaceText acc (to, from) = T.replace from to acc


convertFromSMTString :: String -> String
convertFromSMTString x = T.unpack $ foldl replaceText (T.pack x) replList'
  where replaceText acc (to, from) = T.replace from to acc


--
-- Type.hs ends here
