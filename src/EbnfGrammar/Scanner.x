{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module EbnfGrammar.Scanner(Token, TokenType(..), scan, testScan, AlexPosn(..)) where

import Data.Data(Data)
import Text.StdToken
}

%wrapper "posn"

scanner :-

\:\:\=	{ mkAlexToken YIELDS }
\:	{ mkAlexToken COLON }
\.\.\.	{ mkAlexToken ELLIPSIS }
\.	{ mkAlexToken FULL_STOP }
\{	{ mkAlexToken LEFT_BRACE }
\[	{ mkAlexToken LEFT_BRACKET }
\|	{ mkAlexToken OR }
\}	{ mkAlexToken RIGHT_BRACE }
\}\+	{ mkAlexToken RIGHT_BRACE_PLUS }
\]	{ mkAlexToken RIGHT_BRACKET }
[a-z_][a-z0-9_]*	{ mkAlexToken LOWER_NAME }
[A-Z_][A-Z0-9_]*	{ mkAlexToken UPPER_NAME }


$white+  ;
\#.*$ ;

{

data TokenType = COLON
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

type Token = StdToken TokenType String AlexPosn

deriving instance Data AlexPosn

scan :: String -> [Token]
scan = alexScanTokens

testScan :: IO ()
testScan = do
     src <- readFile "./Eiffel.ebnf"
     print $ scan src
     
-- [ @code ] [ wrapper ] { macrodef } @id ':-' { rule } [ @code ]

}
