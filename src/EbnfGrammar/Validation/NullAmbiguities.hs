module EbnfGrammar.Validation.NullAmbiguities
  ( checkNullAmbiguities
  ) where

import Control.Monad.Except
import Data.Generics.Uniplate.Operations
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import EbnfGrammar.Error
import EbnfGrammar.Syntax
import EbnfGrammar.Utils (monotoneMapFixedPoint)
import SafeMap
import Text.StdToken

checkNullAmbiguities :: Gram -> Either Error Gram
-- figure nullables, check in all the extensions
checkNullAmbiguities g =
  if null ambiguousTerms
    then pure g
    else throwError $ OldError $ NullAmbiguitiesError ambiguousTerms
  where
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
