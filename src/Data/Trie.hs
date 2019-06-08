{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Data.Trie
-- Copyright   : David Johnson (c) 2018-2019
-- License     : All Rights Reserved
-- Maintainer  : David Johnson <djohnson.m@gmail.com>
-- Stability   : Experimental
-- Portability : GHC
--
--------------------------------------------------------------------------------
module Data.Trie
  ( -- * Construction
    singleton
  , fromList
  , emptyTrie
  , insert
  -- * Query
  , null
  , member
  , lookup
  -- * Manipulation
  , delete
   -- * Types
  , Trie (..)
  ) where

import qualified Data.Map        as M
import           Data.Maybe
import           Prelude         hiding (lookup, null)

-- | Empty trie
emptyTrie :: Trie k v
emptyTrie = Trie Nothing M.empty

-- | Data type for working with a Trie
data Trie k v = Trie {
    value    :: Maybe v
    -- ^ optional value at a leaf or parent node
  , children :: M.Map k (Trie k v)
    -- ^ children nodes in Trie, potentially empty
  } deriving (Show, Eq)

-- | Functor instance
instance Functor (Trie k) where
  fmap f Trie {..} = Trie (f <$> value) (fmap f <$> children)

-- | /O(1)/
null :: (Eq k, Eq v) => Trie k v -> Bool
null = (== emptyTrie)

-- | /O(1)/
singleton :: Ord k => k -> v -> Trie k v
singleton k v = insert [k] v emptyTrie

-- | /O(log n)/
member :: Ord k => [k] -> Trie k v -> Bool
member k = isJust . lookup k

-- | /O(log n)/
lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup [] Trie {..} = value
lookup (x:xs) Trie {..} = do
  t <- M.lookup x children
  lookup xs t

-- | /O(log n)/
insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert [] v Trie {..} = Trie (Just v) children
insert (x:xs) v t@Trie {..} =
  t { children = M.insert x (insert xs v k) children }
    where
      k = fromMaybe emptyTrie (M.lookup x children)

-- | /O(log n)/
delete :: Ord k => [k] -> Trie k v -> Trie k v
delete [] t = t { value = Nothing }
delete (x:xs) t@Trie{..} =
  case M.lookup x children of
    Nothing -> t
    Just t2 -> t {
        children = M.insert x (delete xs t2) children
      }

-- | /O(n log n)/
fromList :: Ord k => [([k], v)] -> Trie k v
fromList = foldr (uncurry insert) emptyTrie
