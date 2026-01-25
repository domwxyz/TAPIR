{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Core.Error
-- Description : Error handling utilities and guidelines
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- = Error Handling Guidelines
--
-- == When to use each pattern:
--
-- === 'Maybe' - For expected absence
-- Use when "not found" is a normal, expected case:
--
-- > getSession :: SessionId -> IO (Maybe Session)
--
-- === 'Either TapirError' - For recoverable errors
-- Use when failure should be reported and handled:
--
-- > saveMessage :: Message -> IO (Either TapirError Message)
--
-- === Silent defaults - For non-critical data
-- Use sparingly, always document:
--
-- > textToTags :: Text -> [Text]  -- Returns [] on parse failure (documented)
--
-- === Never use 'error' or partial functions
-- All functions should be total.

module Tapir.Core.Error
  ( -- * Safe List Operations
    safeHead
  , safeLast
  , safeInit
  , safeTail
  , safeIndex
  , safeUnsnoc
  , safeMaximum
  , safeMinimum

    -- * Conversion Helpers
  , noteError
  , fromMaybeError
  ) where

import Data.Text (Text)
import Tapir.Types (TapirError(..))

-- | Safe head - get first element
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Safe last - get last element
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

-- | Safe init - get all elements except the last
safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [_] = Just []
safeInit (x:xs) = (x:) <$> safeInit xs

-- | Safe tail - get all elements except the first
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- | Safe index - get element at index
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
  | n < 0     = Nothing
  | otherwise = safeIndex xs (n - 1)

-- | Safe unsnoc - decompose list into init and last
-- Returns Nothing for empty list, Just (init, last) otherwise
safeUnsnoc :: [a] -> Maybe ([a], a)
safeUnsnoc [] = Nothing
safeUnsnoc [x] = Just ([], x)
safeUnsnoc (x:xs) = case safeUnsnoc xs of
  Nothing -> Nothing  -- Cannot happen due to pattern above, but keeps it total
  Just (ys, y) -> Just (x:ys, y)

-- | Safe maximum - returns Nothing for empty list
safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum xs = Just (maximum xs)

-- | Safe minimum - returns Nothing for empty list
safeMinimum :: Ord a => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just (minimum xs)

-- | Convert Maybe to Either with error
noteError :: TapirError -> Maybe a -> Either TapirError a
noteError err Nothing = Left err
noteError _ (Just a) = Right a

-- | Convert Maybe to Either with custom error constructor
fromMaybeError :: (Text -> TapirError) -> Text -> Maybe a -> Either TapirError a
fromMaybeError mkErr msg Nothing = Left (mkErr msg)
fromMaybeError _ _ (Just a) = Right a
