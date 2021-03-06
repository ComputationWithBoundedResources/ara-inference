-- Exception.hs ---
--
-- Filename: Exception.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Sep  4 10:40:25 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Apr 11 20:57:27 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 65
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

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Rewriting.ARA.Exception.Type
    ( ProgException (..)
    , exceptionPrefixWarning
    , exceptionPrefixFatal
    , exceptionPrefixParse
    , exceptionPrefixSemantic
    , exceptionPrefixUnsolveable
    ) where


import           Control.Exception (Exception)
import           Data.Typeable

-----------------------------------------------------------------------------
-- | Definition of the Exception Types used in this program
data ProgException = ShowTextOnly String
                   | WarningException String
                   | FatalException String
                   | ParseException String
                   | TimeoutException String
                   | UnsolveableException String
                   | SemanticException String
                       deriving (Typeable)


-- | Make the data ProgException an instance of Exception
instance Exception ProgException


instance Show ProgException where
    show (ShowTextOnly x)         = x
    show (SemanticException x)    = exceptionPrefixSemantic ++ x
    show (WarningException x)     = exceptionPrefixWarning ++  x
    show (FatalException x)       = exceptionPrefixFatal ++ x
    show (ParseException x)       = exceptionPrefixParse ++ x
    show (UnsolveableException x) = exceptionPrefixUnsolveable ++ x
    show (TimeoutException x)     = exceptionPrefixTimeout ++ x


-----------------------------------------------------------------------------

-- Constants for module: Data.Rewriting.ARA.Exception.Type
exceptionPrefixWarning :: [Char]
exceptionPrefixWarning = "Warning:"

exceptionPrefixFatal :: [Char]
exceptionPrefixFatal =  "FATAL ERROR:"

exceptionPrefixParse :: [Char]
exceptionPrefixParse =  "Parse Error:"

exceptionPrefixSemantic :: [Char]
exceptionPrefixSemantic = "Semantic Error:"

exceptionPrefixUnsolveable :: [Char]
exceptionPrefixUnsolveable = "Problem unsolvable:"

exceptionPrefixTimeout :: [Char]
exceptionPrefixTimeout = "Timeout: "

--
-- Exception.hs ends here
