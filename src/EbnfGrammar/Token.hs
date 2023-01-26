{-# LANGUAGE DeriveDataTypeable #-}

module EbnfGrammar.Token where

import Data.Data (Data)
import EbnfGrammar.Posn (Posn(..))
import Text.StdToken (StdToken(..))

data TokenType
  = COLON
  | ELLIPSIS
  | FULL_STOP
  | LEFT_BRACE
  | LEFT_BRACKET
  | LOWER_NAME
  | OR
  | RIGHT_BRACE
  | RIGHT_BRACE_PLUS
  | RIGHT_BRACKET
  | UPPER_NAME
  | YIELDS
  deriving (Data, Eq, Ord, Show)

type Token = StdToken TokenType String Posn
