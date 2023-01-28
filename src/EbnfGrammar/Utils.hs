{-# LANGUAGE ScopedTypeVariables #-}

module EbnfGrammar.Utils
  ( chooseOne
  , collectOnFirst
  , monotoneFixedPoint
  , monotoneMapFixedPoint
  ) where

import Data.List (find, nub, sort)
import qualified Data.Map as M

chooseOne :: [p] -> [(p, [p])]
chooseOne [] = []
chooseOne [p] = [(p, [])]
chooseOne (p:ps) = (p, ps) : fmap (fmap (p :)) (chooseOne ps)

collectOnFirst ::
     forall a b. Ord a
  => [(a, b)]
  -> [(a, [b])]
collectOnFirst abs' = [(a, [b' | (a', b') <- abs', a' == a]) | a <- as']
  where
    as' :: [a]
    as' = sort $ nub $ map fst abs'

monotoneFixedPoint ::
     forall a. Eq a
  => (a -> a)
  -> a
  -> a
monotoneFixedPoint f a = maybe err fst $ find (uncurry (==)) pairs
  where
    monotoneSeq :: [a]
    monotoneSeq = iterate f a
    pairs = zip monotoneSeq (tail monotoneSeq)
    err = error "error: monotoneFixedPoint never converged"

monotoneMapFixedPoint ::
     forall k v. (Eq k, Eq v)
  => (M.Map k v -> k -> v)
  -> M.Map k v
  -> M.Map k v
monotoneMapFixedPoint f = monotoneFixedPoint f'
  where
    f' :: M.Map k v -> M.Map k v
    f' m = M.fromSet (f m) (M.keysSet m)
