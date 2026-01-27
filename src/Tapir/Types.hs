{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Types
-- Description : Re-export hub for all domain types
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- This module re-exports types from domain-specific modules for
-- backward compatibility. New code should import from specific
-- modules when only a subset of types is needed.

module Tapir.Types
  ( -- * Chat Domain
    module Tapir.Model.Chat

    -- * Anki Domain
  , module Tapir.Model.Anki

    -- * Errors
  , module Tapir.Model.Error

    -- * Mode
  , module Tapir.Types.Mode

    -- * Language
  , module Tapir.Types.Language

    -- * Provider
  , module Tapir.Types.Provider
  ) where

import Tapir.Model.Chat
import Tapir.Model.Anki
import Tapir.Model.Error
import Tapir.Types.Mode
import Tapir.Types.Language
import Tapir.Types.Provider
