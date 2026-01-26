{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Core.Selection
-- Description : Type-safe indexed selection over non-empty lists
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module provides a type-safe way to represent a selection from a list,
-- guaranteeing that the selected index is always valid.

module Tapir.Core.Selection
  ( -- * Selection Type
    Selection
  , SelectionEmpty(..)

    -- * Construction
  , fromList
  , fromNonEmpty
  , singleton

    -- * Queries
  , selected
  , selectedIndex
  , toList
  , length
  , isEmpty

    -- * Navigation
  , moveNext
  , movePrev
  , moveToIndex
  , moveToStart
  , moveToEnd

    -- * Modification
  , updateItems
  , filter
  , mapItems
  ) where

import Prelude hiding (length, filter)
import qualified Prelude
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)

-- | A non-empty list with a selected element.
-- Invariant: 0 <= selIndex < length selItems
data Selection a = Selection
  { selItems :: !(NonEmpty a)
  , selIndex :: !Int
  } deriving (Eq, Show, Generic)

-- | Result when trying to create a Selection from an empty list
data SelectionEmpty = SelectionEmpty
  deriving (Eq, Show)

-- | Create a Selection from a regular list. Returns Left SelectionEmpty if empty.
-- The first element is selected by default.
fromList :: [a] -> Either SelectionEmpty (Selection a)
fromList [] = Left SelectionEmpty
fromList (x:xs) = Right $ Selection (x :| xs) 0

-- | Create a Selection from a NonEmpty list.
-- The first element is selected by default.
fromNonEmpty :: NonEmpty a -> Selection a
fromNonEmpty ne = Selection ne 0

-- | Create a Selection with a single item.
singleton :: a -> Selection a
singleton x = Selection (x :| []) 0

-- | Get the currently selected item.
--
-- This is guaranteed safe because the Selection invariant ensures
-- 0 <= selIndex < length selItems. We use an explicit fold rather
-- than (!!) to satisfy static analysis tools and provide defense
-- in depth.
selected :: Selection a -> a
selected (Selection items idx) =
  case drop idx (NE.toList items) of
    (x:_) -> x
    []    -> NE.head items  -- Invariant violation fallback

-- | Get the current selection index (0-based).
selectedIndex :: Selection a -> Int
selectedIndex = selIndex

-- | Convert back to a regular list.
toList :: Selection a -> [a]
toList = NE.toList . selItems

-- | Get the number of items.
length :: Selection a -> Int
length = Prelude.length . NE.toList . selItems

-- | Check if selection has only one item.
isEmpty :: Selection a -> Bool
isEmpty s = length s == 0  -- Can't actually happen due to NonEmpty, but included for API completeness

-- | Move selection to the next item. Stays at end if already at last item.
moveNext :: Selection a -> Selection a
moveNext s@(Selection items idx)
  | idx >= length s - 1 = s
  | otherwise = Selection items (idx + 1)

-- | Move selection to the previous item. Stays at start if already at first item.
movePrev :: Selection a -> Selection a
movePrev s@(Selection items idx)
  | idx <= 0 = s
  | otherwise = Selection items (idx - 1)

-- | Move to a specific index. Clamps to valid range.
moveToIndex :: Int -> Selection a -> Selection a
moveToIndex targetIdx s@(Selection items _) =
  let maxIdx = length s - 1
      clampedIdx = max 0 (min targetIdx maxIdx)
  in Selection items clampedIdx

-- | Move to the first item.
moveToStart :: Selection a -> Selection a
moveToStart (Selection items _) = Selection items 0

-- | Move to the last item.
moveToEnd :: Selection a -> Selection a
moveToEnd s@(Selection items _) = Selection items (length s - 1)

-- | Update the items, trying to preserve selection index.
-- If new list is empty, returns Left SelectionEmpty.
-- If index is now out of bounds, clamps to last item.
updateItems :: [a] -> Selection a -> Either SelectionEmpty (Selection a)
updateItems [] _ = Left SelectionEmpty
updateItems (x:xs) (Selection _ oldIdx) =
  let newItems = x :| xs
      maxIdx = Prelude.length xs  -- length of tail = max valid index
      newIdx = min oldIdx maxIdx
  in Right $ Selection newItems newIdx

-- | Filter items, trying to preserve selection.
-- Returns Left SelectionEmpty if all items are filtered out.
filter :: (a -> Bool) -> Selection a -> Either SelectionEmpty (Selection a)
filter p s =
  let filtered = Prelude.filter p (toList s)
  in fromList filtered

-- | Map a function over all items.
mapItems :: (a -> b) -> Selection a -> Selection b
mapItems f (Selection items idx) = Selection (NE.map f items) idx
