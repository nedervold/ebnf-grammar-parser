{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EbnfGrammar.Posn
  ( Posn(..)
  ) where

import Data.Data (Data)
import Prettyprinter
import Text.Printf (printf)

data Posn
  = Posn
      { charOffset :: !Int
      , lineNumber :: !Int
      , columnNumber :: !Int
      }
  | EOF
  deriving (Data, Eq, Ord, Show)

instance Pretty Posn where
  pretty Posn {..} =
    pretty (printf "Line %d, column %d" lineNumber columnNumber :: String)
  pretty EOF = "<eof>"
