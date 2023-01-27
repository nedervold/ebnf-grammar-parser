module EbnfGrammar.Validation.UniqueConstructors
  ( checkUniqueConstructors
  ) where

import Control.Monad.Except
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import EbnfGrammar.Error
import EbnfGrammar.Posn (Posn)
import EbnfGrammar.Syntax
import EbnfGrammar.Utils (collectOnFirst)
import Text.StdToken

checkUniqueConstructors :: Gram -> Either Error Gram
checkUniqueConstructors g@(Gram ps) =
  if null multiples
    then pure g
    else throwError $ NonUniqueConstructors multiples
  where
    multiples :: [(String, [Posn])]
    multiples = fmap (fmap sort) $ filter isMultiple $ collectOnFirst pairs
    isMultiple :: (s, [p]) -> Bool
    isMultiple = (> 1) . length . snd
    pairs :: [(String, Posn)]
    pairs =
      [ (_tokenText ctor, _tokenDeco ctor)
      | Prod hd alts <- NE.toList ps
      , Alt mCtor _ <- NE.toList alts
      , let ctor = fromMaybe hd mCtor
      ]
