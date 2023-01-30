{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Validation.UnusedVocab
  ( checkUnusedVocab
  ) where

import Algebra.Graph.AdjacencyMap (edges, vertexSet)
import Algebra.Graph.AdjacencyMap.Algorithm (reachable)
import Control.Monad.Except (MonadError)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import EbnfGrammar.Error
  ( Error(..)
  , ErrorType(UnreachableError)
  , Errors
  , throwErrorList
  )
import EbnfGrammar.Posn (Posn)
import EbnfGrammar.Syntax
import EbnfGrammar.Token (StdToken(..))
import Prettyprinter

checkUnusedVocab :: MonadError Errors m => Gram -> m Gram
-- make a graph and check reachable from the root
checkUnusedVocab g@(Gram ps) =
  if S.null unreachables
    then pure g
    else throwErrorList $
         [ Error posn UnreachableError $
         hsep ["The", "nonterminal", pretty $ show str, "is", "unreachable."]
         | (posn, str) <- locatedUnreachables
         ]
  where
    locatedUnreachables :: [(Posn, String)]
    locatedUnreachables =
      [ (_tokenDeco hd', nm)
      | Prod hd' _ <- NE.toList ps
      , let nm = _tokenText hd'
      , nm `S.member` unreachables
      ]
    unreachables :: S.Set String
    unreachables = allSymbols S.\\ reachables
      where
        allSymbols = vertexSet gr
    Prod hd _ = NE.head ps
    startSym = _tokenText hd
    reachables :: S.Set String
    reachables = S.fromList $ reachable startSym gr
    gr = edges es
    es :: [(String, String)]
    es =
      [ (_tokenText hd', vocabText v)
      | Prod hd' alts <- NE.toList ps
      , Alt _mCtor ts <- NE.toList alts
      , t <- ts
      , v <- termVocab t
      ]
    termVocab :: Term -> [Vocab]
    termVocab (VocabTerm v) = pure v
    termVocab (Opt v) = pure v
    termVocab (Rep0 v) = pure v
    termVocab (Rep1 v) = pure v
    termVocab (Repsep0 b s) = [b, s]
    termVocab (Repsep1 b s) = [b, s]
    vocabText :: Vocab -> String
    vocabText (NT tok) = _tokenText tok
    vocabText (T tok) = _tokenText tok
