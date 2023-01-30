{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module EbnfGrammar.Scanner
  ( scan, scan'
  ) where

import Control.Monad.Except(throwError)
import Data.Bifunctor(first)
import EbnfGrammar.Error (Error(..), ErrorType(..), Errors(..))
import qualified Data.Set as S
import EbnfGrammar.Posn (Posn(..))
import EbnfGrammar.Token (Token, TokenType(..))
import Prettyprinter hiding (column, line)
import Text.StdToken (StdToken(..))
}

%wrapper "posn"

scanner :-

\:\:\=	{ mkToken YIELDS }
\:	{ mkToken COLON }
\.\.\.	{ mkToken ELLIPSIS }
\.	{ mkToken FULL_STOP }
\{	{ mkToken LEFT_BRACE }
\[	{ mkToken LEFT_BRACKET }
\|	{ mkToken OR }
\}\+	{ mkToken RIGHT_BRACE_PLUS }
\}	{ mkToken RIGHT_BRACE }
\]	{ mkToken RIGHT_BRACKET }
[a-z][a-z0-9_]*	{ mkToken LOWER_NAME }
[A-Z][A-Z0-9_]*	{ mkToken UPPER_NAME }

$white+  ;
\#.*$ ;

. { scanError }
{
mkToken :: TokenType -> AlexPosn -> String -> Either (FilePath -> Errors) Token
mkToken tt (AlexPn o l c) txt = Right $ Token tt txt (Posn o l c)

scanError :: AlexPosn -> String -> Either (FilePath -> Errors) Token
scanError (AlexPn o l c) txt =
  throwError $ \ fp -> Errors $ S.singleton $
  Error
    (Posn o l c)
    ScanError
    (hsep ["Scan", "error", "at", "character", pretty (show $ head txt) <> "."])

scan' :: String -> Either (FilePath -> Errors) [Token]
scan' = sequenceA . alexScanTokens

scan :: FilePath -> String -> Either Errors [Token]
scan fp src = first ($ fp) $ scan' src
}
