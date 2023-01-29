{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Validation.NullAmbiguities
  ( checkNullAmbiguities
  ) where

import Control.Monad.Except
import Data.Generics.Uniplate.Operations
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

checkNullAmbiguities :: Gram -> Either Errors Gram
checkNullAmbiguities g =
  if null ambiguousTerms
    then pure g
    else throwError $
         Errors $
         S.fromList
           [ Error posn AmbiguousNullableError' msg
           | term <- ambiguousTerms
           , let posn = termPosn term
           , let msg = termMsg term
           ]
  where
    termMsg :: Term -> Doc ann
    termMsg t =
      hsep
        [ "This"
        , "term"
        , "with"
        , "nullable"
        , "contents"
        , "is"
        , "ambiguous:"
        , pretty t <> "."
        ]
    termPosn :: Term -> Posn
    termPosn t =
      case t of
        VocabTerm v -> vocabPosn v
        Opt v -> vocabPosn v
        Rep0 v -> vocabPosn v
        Rep1 v -> vocabPosn v
        Repsep0 v _ -> vocabPosn v
        Repsep1 v _ -> vocabPosn v
    vocabPosn (NT tok) = _tokenDeco tok
    vocabPosn (T tok) = _tokenDeco tok
    ambiguousTerms :: [Term]
    ambiguousTerms = filter isAmbiguous $ universeBi g
    nullableNTs :: S.Set String
    nullableNTs = S.fromList [str | (str, True) <- M.toList $ nullables g]
    isAmbiguous :: Term -> Bool
    isAmbiguous (Opt b) =
      case b of
        NT tok -> _tokenText tok `S.member` nullableNTs
        T _ -> False
    isAmbiguous (Rep0 b) =
      case b of
        NT tok -> _tokenText tok `S.member` nullableNTs
        T _ -> False
    isAmbiguous (Repsep0 b _) =
      case b of
        NT tok -> _tokenText tok `S.member` nullableNTs
        T _ -> False
    isAmbiguous _ = False

nullables :: Gram -> M.Map String Bool
nullables (Gram ps) = monotoneMapFixedPoint f initMap
  where
    ntLookup :: M.Map String Prod
    ntLookup =
      M.fromList [(_tokenText hd, prod) | prod@(Prod hd _) <- NE.toList ps]
    initMap :: M.Map String Bool
    initMap = False <$ ntLookup
    f :: M.Map String Bool -> String -> Bool
    f m nt = m ! nt || isNullableProd (ntLookup ! nt)
      where
        isNullableProd (Prod _ alts) = any isNullableAlt alts
        isNullableAlt (Alt _ ts) = all isNullableTerm ts
        isNullableTerm t =
          case t of
            VocabTerm v -> isNullableVocab v
            Opt _ -> True
            Rep0 _ -> True
            Rep1 b -> isNullableVocab b
            Repsep0 _ _ -> True
            Repsep1 b _ -> isNullableVocab b
        isNullableVocab (T _) = False
        isNullableVocab (NT tok) = m ! _tokenText tok
