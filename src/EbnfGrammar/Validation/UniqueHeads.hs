module EbnfGrammar.Validation.UniqueHeads
  ( checkUniqueHeads
  ) where

import Control.Monad.Except (throwError)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import EbnfGrammar.Error
import EbnfGrammar.Posn
import EbnfGrammar.Syntax
import EbnfGrammar.Utils
import Text.StdToken

checkUniqueHeads :: Gram -> Either Error Gram
checkUniqueHeads g@(Gram ps) =
  if null multiples
    then pure g
    else throwError $ NonUniqueHeads multiples
  where
    multiples :: [(String, [Posn])]
    multiples = fmap (fmap sort) $ filter isMultiple $ collectOnFirst pairs
    isMultiple :: (s, [p]) -> Bool
    isMultiple = (> 1) . length . snd
    pairs :: [(String, Posn)]
    pairs = [(_tokenText hd, _tokenDeco hd) | Prod hd _ <- NE.toList ps]
