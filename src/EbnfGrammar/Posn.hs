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
      { posnFilePath :: FilePath
      , posnOffset :: !Int
      , posnLine :: !Int
      , posnColumn :: !Int
      }
  | EOF
      { posnFilePath :: FilePath
      }
  deriving (Data, Eq, Ord, Show)

instance Pretty Posn where
  pretty posn =
    prettyStr $
    case posn of
      Posn {..} -> printf "%s:%d:%d" posnFilePath posnLine posnColumn
      EOF {..} -> printf "%s:<eof>" posnFilePath

prettyStr :: String -> Doc ann
prettyStr = pretty
