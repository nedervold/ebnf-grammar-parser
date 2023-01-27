module EbnfGrammar.Validation.NullAmbiguities
  ( checkNullAmbiguities
  ) where

import Control.Monad.Except
import Data.Generics.Uniplate.Operations
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import EbnfGrammar.Error
import EbnfGrammar.Syntax
import EbnfGrammar.Utils (monotoneFixedPoint)
import Text.StdToken

checkNullAmbiguities :: Gram -> Either Error Gram
-- figure nullables, check in all the extensions
checkNullAmbiguities g =
  if null ambiguousTerms
    then pure g
    else throwError $ NullAmbiguitiesError ambiguousTerms
  where
    ambiguousTerms :: [Term]
    ambiguousTerms = filter isAmbiguous $ universeBi g
    nullableNTs :: S.Set String
    nullableNTs = nullables g
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

nullables :: Gram -> S.Set String
nullables (Gram ps) = monotoneFixedPoint calcNullables S.empty
  where
    calcNullables :: S.Set String -> S.Set String
    calcNullables = flip (foldl' calcNullable) $ NE.toList ps
      where
        calcNullable :: S.Set String -> Prod -> S.Set String
        calcNullable nullables' (Prod hd alts)
          | hdSym `S.member` nullables' = nullables'
          | any isNullableAlt (NE.toList alts) = S.insert hdSym nullables'
          | otherwise = nullables'
          where
            hdSym = _tokenText hd
            isNullableAlt :: Alt -> Bool
            isNullableAlt (Alt _ ts) = all isNullableTerm ts
            isNullableTerm :: Term -> Bool
            isNullableTerm t =
              case t of
                VocabTerm v -> isNullableVocab v
                Rep1 v -> isNullableVocab v
                Repsep1 b _ -> isNullableVocab b
                _ -> True
            isNullableVocab :: Vocab -> Bool
            isNullableVocab v =
              case v of
                T _ -> False
                NT tok -> _tokenText tok `S.member` nullables'
