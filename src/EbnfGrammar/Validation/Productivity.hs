{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module EbnfGrammar.Validation.Productivity
  ( checkProductivity
  ) where

import Control.Monad.Except (throwError)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import EbnfGrammar.Error
import EbnfGrammar.Syntax
import EbnfGrammar.Utils (monotoneMapFixedPoint)
import Text.StdToken

initMap :: Gram -> M.Map PA Bool
initMap (Gram ps) =
  M.fromSet (const False) $ S.fromList $ concatMap getPAs $ NE.toList ps
  where
    getPAs :: Prod -> [PA]
    getPAs prod@(Prod _hd alts) = P prod : [A alt | alt <- NE.toList alts]

checkProductivity :: Gram -> Either Error Gram
checkProductivity gram =
  if S.null unproductives
    then pure gram
    else throwError $ OldError $ UnproductiveError unproductives
  where
    unproductives :: S.Set PA
    unproductives = S.fromList [pa | (pa, False) <- M.toList productiveMap]
    productiveMap :: M.Map PA Bool
    productiveMap = monotoneMapFixedPoint calcProductives $ initMap gram
    calcProductives :: M.Map PA Bool -> PA -> Bool
    calcProductives m pa =
      m M.! pa ||
      case pa of
        P (Prod _ alts) -> any (\alt -> m M.! A alt) alts
        A (Alt _ ts) -> all isProductiveTerm ts
      where
        isProductiveTerm :: Term -> Bool
        isProductiveTerm t =
          case t of
            VocabTerm v -> isProductiveVocab v
            Opt _ -> True
            Rep0 _ -> True
            Rep1 v -> isProductiveVocab v
            Repsep0 _ _ -> True
            Repsep1 b _ -> isProductiveVocab b
        isProductiveVocab :: Vocab -> Bool
        isProductiveVocab (T _) = True
        isProductiveVocab (NT tok) = nt `S.member` productiveNTs m
          where
            nt = _tokenText tok

productiveNTs :: M.Map PA Bool -> S.Set String
productiveNTs m =
  S.fromList [_tokenText hd | (P (Prod hd _), True) <- M.toList m]
