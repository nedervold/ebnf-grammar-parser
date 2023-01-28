module EbnfGrammar.Validation.UnusedVocab
  ( checkUnusedVocab
  ) where

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
import Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import EbnfGrammar.Error
import EbnfGrammar.Syntax
import Text.StdToken

checkUnusedVocab :: Gram -> Either Error Gram
-- make a graph and check reachable from the root
checkUnusedVocab g@(Gram ps) =
  if S.null unreachables
    then pure g
    else throwError $ OldError $ UnreachableError unreachables
  where
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
