module EbnfGrammar.Validation.Productivity
  ( checkProductivity
  ) where

import Control.Monad.Except
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import EbnfGrammar.Error
import EbnfGrammar.Syntax
import EbnfGrammar.Utils (ensureCtors, monotoneFixedPoint)
import Text.StdToken

-- TODO If I've already determined there are no duplicate heads...  I
-- can define Ord via the head.
checkProductivity :: Gram -> Either Error Gram
-- make a table of prods and alts and recurse to a fixed point
checkProductivity g =
  if S.null unproductives
    then pure g
    else throwError $ UnproductiveError unproductives
  where
    unproductives :: S.Set PA
    unproductives = allPA S.\\ productives
    Gram ps = ensureCtors g
    allPA :: S.Set PA
    allPA =
      S.fromList $
      concat
        [ P prod : [A alt | alt <- NE.toList alts]
        | prod@(Prod _hd alts) <- NE.toList ps
        ]
    productives :: S.Set PA
    productives = monotoneFixedPoint calcProductives S.empty
    calcProductives :: S.Set PA -> S.Set PA
    calcProductives = foldl' calcProductive allPA
      where
        calcProductive :: S.Set PA -> PA -> S.Set PA
        calcProductive isProductive pa =
          if pa `S.member` isProductive
            then isProductive
            else let paIsProductive =
                       case pa of
                         P (Prod _hd alts) ->
                           any ((`S.member` isProductive) . A) $ NE.toList alts
                         A (Alt _ terms) -> all isProductiveTerm terms
                  in if paIsProductive
                       then S.insert pa isProductive
                       else isProductive
          where
            isProductiveTerm :: Term -> Bool
            isProductiveTerm (VocabTerm v) = isProductiveVocab v
            isProductiveTerm (Opt _) = True
            isProductiveTerm (Rep0 _) = True
            isProductiveTerm (Rep1 v) = isProductiveVocab v
            isProductiveTerm (Repsep0 _ _) = True
            isProductiveTerm (Repsep1 b _) = isProductiveVocab b
            isProductiveVocab :: Vocab -> Bool
            isProductiveVocab (T _) = True
            isProductiveVocab (NT tok) = isProductiveProdName $ _tokenText tok
            isProductiveProdName :: String -> Bool
            isProductiveProdName str =
              str `elem`
              [_tokenText hd | P (Prod hd _) <- S.toList isProductive]
