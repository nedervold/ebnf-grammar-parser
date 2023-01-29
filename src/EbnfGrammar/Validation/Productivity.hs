{-# LANGUAGE OverloadedStrings #-}
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
import EbnfGrammar.Posn
import EbnfGrammar.Syntax
import EbnfGrammar.Utils (monotoneMapFixedPoint)
import Prettyprinter
import SafeMap
import Text.StdToken

initMap :: Gram -> M.Map PA (Posn, Bool)
initMap (Gram ps) = M.fromSet f $ S.fromList $ concatMap getPAs $ NE.toList ps
  where
    getPAs :: Prod -> [PA]
    getPAs prod@(Prod _hd alts) = P prod : [A alt | alt <- NE.toList alts]
    f :: PA -> (Posn, Bool)
    f (P (Prod hd _)) = (_tokenDeco hd, False)
    f (A (Alt ctor _)) = (_tokenDeco ctor, False)

checkProductivity :: Gram -> Either Errors Gram
checkProductivity gram =
  if S.null unproductives
    then pure gram
    else throwError $
         Errors $
         S.fromList
           [ Error posn UnproductiveError' msg
           | pa <- S.toList unproductives
           , let posn = posnMap M.! pa
           , let msg = mkMsg pa
           ]
  where
    mkMsg (P (Prod hd _)) =
      hsep ["Production", pretty $ show $ _tokenText hd, "is", "unproductive."]
    mkMsg (A (Alt ctor _)) =
      hsep
        [ "Alternative"
        , "with"
        , "constructor"
        , pretty $ show $ _tokenText ctor
        , "is"
        , "unproductive."
        ]
    posnMap = fmap fst iMap
    unproductives :: S.Set PA
    unproductives = S.fromList [pa | (pa, False) <- M.toList productiveMap]
    iMap = initMap gram
    productiveMap :: M.Map PA Bool
    productiveMap = monotoneMapFixedPoint calcProductives $ fmap snd iMap
    calcProductives :: M.Map PA Bool -> PA -> Bool
    calcProductives m pa =
      m ! pa ||
      case pa of
        P (Prod _ alts) -> any (\alt -> m ! A alt) alts
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
