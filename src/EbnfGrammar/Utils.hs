-- | Utility functions.
{-# LANGUAGE ScopedTypeVariables #-}

module EbnfGrammar.Utils
  ( chooseOne
  , collectOnFirst
  , monotoneFixedPoint
  , monotoneMapFixedPoint
  ) where

import Data.List (find, nub, sort)
import qualified Data.Map as M

-- | From a list of items, choose an item from the list and pair it
-- with the remaining items.  Preserves the ordering.
chooseOne :: [p] -> [(p, [p])]
chooseOne [] = []
chooseOne [p] = [(p, [])]
chooseOne (p:ps) = (p, ps) : fmap (fmap (p :)) (chooseOne ps)

-- | Collects all the 'b's for their matching 'a'.  Preserves the
-- order of both the 'a's and their 'b's.
collectOnFirst ::
     forall a b. Ord a
  => [(a, b)]
  -> [(a, [b])]
collectOnFirst abs' = [(a, [b' | (a', b') <- abs', a' == a]) | a <- as']
  where
    as' :: [a]
    as' = sort $ nub $ map fst abs'

-- | Runs the action on the initial value until it no longer changes.
monotoneFixedPoint ::
     forall a. Eq a
  => (a -> a) -- ^ the action
  -> a -- ^ the initial value
  -> a
monotoneFixedPoint f a = maybe err fst $ find (uncurry (==)) pairs
  where
    monotoneSeq :: [a]
    monotoneSeq = iterate f a
    pairs = zip monotoneSeq (tail monotoneSeq)
    err = error "error: monotoneFixedPoint never converged"

-- | Using the function to calculate a new value for a key, updates
-- the whole map repeatedly until it stops changing.
monotoneMapFixedPoint ::
     forall k v. (Eq k, Eq v)
  => (M.Map k v -> k -> v) -- ^ calculate new value for key
  -> M.Map k v -- ^ initial map
  -> M.Map k v
monotoneMapFixedPoint f = monotoneFixedPoint f'
  where
    f' :: M.Map k v -> M.Map k v
    f' m = M.fromSet (f m) (M.keysSet m)
