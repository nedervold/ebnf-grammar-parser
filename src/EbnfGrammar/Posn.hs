{-# LANGUAGE DeriveDataTypeable #-}

module EbnfGrammar.Posn
  ( Posn(..)
  ) where

import Data.Data (Data)

data Posn =
  Posn
    { charOffset :: !Int
    , lineNumber :: !Int
    , columnNumber :: !Int
    }
  deriving (Data, Eq, Ord, Show)
