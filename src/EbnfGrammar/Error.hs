{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module EbnfGrammar.Error
  ( Error(..)
  , Errors(..)
  , throwErrors
  , ErrorType(..)
  ) where

import Control.Monad.Except
import qualified Data.Set as S
import EbnfGrammar.Posn
import EbnfGrammar.Prettyprinter ()
import Prettyprinter

newtype Errors =
  Errors
    { unErrors :: S.Set Error
    }
  deriving (Semigroup, Monoid)

throwErrors :: MonadError Errors m => Error -> m a
throwErrors = throwError . Errors . S.singleton

instance Pretty Errors where
  pretty errs = vcat $ map pretty $ S.toList $ unErrors errs

data ErrorType
  = ScanError'
  | ParseError'
  | DuplicateHeadError'
  | DuplicateConstructorError'
  | UndefinedNonterminalError'
  | UnproductiveError'
  | UnreachableError'
  | AmbiguousNullableError'
  deriving (Eq, Ord, Show)

data Error =
  Error
    { errorPosn :: Posn
    , errorType :: ErrorType
    , errorText :: forall ann. Doc ann
    }

instance Eq Error where
  e == e' = (errorPosn e, errorType e) == (errorPosn e', errorType e')

instance Ord Error where
  compare e e' = compare (errorPosn e, errorType e) (errorPosn e', errorType e')

instance Pretty Error where
  pretty Error {..} =
    vsep
      [ hsep [pretty errorPosn, pretty (show errorType ++ ":")]
      , indent 2 errorText
      ]
