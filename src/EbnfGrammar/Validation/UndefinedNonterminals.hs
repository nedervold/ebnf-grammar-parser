{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Validation.UndefinedNonterminals
  ( checkUndefinedNonterminals
  ) where

import Control.Monad.Except (MonadError)
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Operations (universeBi)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import EbnfGrammar.Error
  ( Error(..)
  , ErrorType(UndefinedNonterminalError)
  , Errors
  , throwErrorList
  )
import EbnfGrammar.Syntax
import EbnfGrammar.Token (StdToken(..))
import Prettyprinter

checkUndefinedNonterminals :: MonadError Errors m => Gram -> m Gram
checkUndefinedNonterminals g@(Gram ps) =
  if S.null undefinedNonterminals
    then pure g
    else throwErrorList
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
