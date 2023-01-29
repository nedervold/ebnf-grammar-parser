{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Validation.UniqueConstructors
  ( checkUniqueConstructors
  ) where

import Control.Monad.Except
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import EbnfGrammar.Error
import EbnfGrammar.Posn (Posn)
import EbnfGrammar.Syntax
import EbnfGrammar.Utils (chooseOne, collectOnFirst)
import Prettyprinter
import Text.Printf (printf)
import Text.StdToken

checkUniqueConstructors :: Gram -> Either Errors Gram
checkUniqueConstructors g@(Gram ps) =
  if null multiples
    then pure g
    else throwError $
         Errors $
         S.fromList
           [ Error posn DuplicateConstructorError' msg
           | (ctor, posns) <- multiples
           , (posn, posns') <- chooseOne posns
           , let msg =
                   printf
                     "%s also defined at %s."
                     (show ctor)
                     (show (sep $ punctuate "," $ map pretty posns') :: String)
           ]
  where
    multiples :: [(String, [Posn])]
    multiples = fmap (fmap sort) $ filter isMultiple $ collectOnFirst pairs
    isMultiple :: (s, [p]) -> Bool
    isMultiple = (> 1) . length . snd
    pairs :: [(String, Posn)]
    pairs =
      [ (_tokenText ctor, _tokenDeco ctor)
      | Prod _hd alts <- NE.toList ps
      , Alt ctor _ <- NE.toList alts
      ]
