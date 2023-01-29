{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Validation.UndefinedNonterminals
  ( checkUndefinedNonterminals
  ) where

import Control.Monad.Except
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Operations
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import EbnfGrammar.Error
import EbnfGrammar.Syntax
import Prettyprinter
import Text.StdToken

checkUndefinedNonterminals :: Gram -> Either Errors Gram
checkUndefinedNonterminals g@(Gram ps) =
  if S.null undefinedNonterminals
    then pure g
    else throwError $
         Errors $
         S.fromList
           [ Error
             (_tokenDeco tok)
             UndefinedNonterminalError
             (hsep ["Nonterminal", pretty $ show nm, "is", "undefined."])
           | NT tok <- universeBi g
           , let nm = _tokenText tok
           , nm `S.member` undefinedNonterminals
           ]
  where
    defined :: [String]
    altss :: [[Alt]]
    (defined, altss) =
      unzip $
      map (\(Prod hd' alts') -> (_tokenText hd', NE.toList alts')) $
      NE.toList ps
    definedNonterminals :: S.Set String
    definedNonterminals = S.fromList defined
    usedNonterminals :: S.Set String
    usedNonterminals =
      S.fromList
        [_tokenText tok | NT tok <- concatMap universeBi $ concat altss]
    undefinedNonterminals :: S.Set String
    undefinedNonterminals = usedNonterminals S.\\ definedNonterminals
