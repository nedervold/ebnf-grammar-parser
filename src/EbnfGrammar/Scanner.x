{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module EbnfGrammar.Scanner
  ( scan
  ) where

import Control.Monad.Except(throwError)
import Control.Monad.Reader(ReaderT, runReaderT, ask)
import EbnfGrammar.Error (Error(..), ErrorType(..), Errors(..))
import qualified Data.Set as S
import EbnfGrammar.Posn (Posn(..))
import EbnfGrammar.Token (Token, TokenType(..), StdToken(Token))
import Prettyprinter hiding (column, line)
import EbnfGrammar.Token(Token(..))
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
type R = ReaderT FilePath (Either Errors)

mkToken :: TokenType -> AlexPosn -> String -> R Token
mkToken tt (AlexPn o l c) txt = do
    fp <- ask
    pure $ Token tt txt (Posn fp o l c)

scanError :: AlexPosn -> String -> R Token
scanError (AlexPn o l c) txt = do
  fp <- ask
  throwError $ Errors $ S.singleton $   Error
    (Posn fp o l c)
    ScanError
    (hsep ["Scan", "error", "at", "character", pretty (show $ head txt) <> "."])

scan :: FilePath -> String -> Either Errors [Token]
scan fp src = runReaderT (sequenceA $ alexScanTokens src) fp
}
