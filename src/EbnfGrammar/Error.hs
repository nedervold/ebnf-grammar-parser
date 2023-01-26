{-# LANGUAGE DeriveDataTypeable #-}

module EbnfGrammar.Error
  ( Posn(..)
  , Error(..)
  ) where

import Data.Data (Data)

data Posn =
  Posn
    { charOffset :: !Int
    , lineNumber :: !Int
    , columnNumber :: !Int
    }
  deriving (Data, Eq, Ord, Show)

data Error
  = ScanError
      { scanErrorPosn :: Posn
      , scanErrorText :: String
      }
  | ParseError
      { parseErrorPosn :: Maybe Posn
      , parseErrorText :: String
      }
  deriving (Show)
