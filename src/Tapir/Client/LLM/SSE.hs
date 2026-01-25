{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Tapir.Client.LLM.SSE
-- Description : Server-Sent Events stream processing
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT
--
-- Shared SSE stream processing for all LLM providers that use
-- the OpenAI-compatible streaming format.

module Tapir.Client.LLM.SSE
  ( processSSEStream
  ) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad (unless)
import Data.Aeson (decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, modifyIORef')
import Network.HTTP.Client (BodyReader, brRead)

import Tapir.Client.LLM.Types (StreamChunk)

-- | Process a Server-Sent Events stream
--
-- Handles the SSE protocol used by OpenAI-compatible APIs:
-- - Lines prefixed with "data: " contain JSON chunks
-- - "data: [DONE]" signals end of stream
-- - Empty lines and comments (starting with ':') are ignored
processSSEStream
  :: BodyReader
  -> Maybe (TVar Bool)           -- ^ Optional cancel flag
  -> (StreamChunk -> IO ())      -- ^ Chunk handler
  -> IO ()
processSSEStream bodyReader mCancelFlag onChunk = do
  -- Buffer for incomplete lines
  bufferRef <- newIORef BS.empty

  let loop = do
        -- Check for cancellation
        cancelled <- case mCancelFlag of
          Just flag -> readTVarIO flag
          Nothing   -> pure False

        unless cancelled $ do
          -- Read next chunk of data
          chunk <- brRead bodyReader

          unless (BS.null chunk) $ do
            -- Append to buffer
            buffer <- readIORef bufferRef
            let fullBuffer = buffer <> chunk

            -- Process complete lines
            let (completeLines, remainder) = splitLines fullBuffer
            modifyIORef' bufferRef (const remainder)

            -- Process each line
            mapM_ processLine completeLines

            -- Continue reading
            loop

  loop
  where
    -- Split buffer into complete lines and remainder
    splitLines :: ByteString -> ([ByteString], ByteString)
    splitLines bs =
      let parts = BS8.split '\n' bs
      in case parts of
        []  -> ([], BS.empty)
        [x] -> ([], x)  -- Incomplete line
        xs  -> (init xs, last xs)

    -- Process a single SSE line
    processLine :: ByteString -> IO ()
    processLine line
      -- Skip empty lines
      | BS.null line = pure ()
      -- Skip SSE comments (lines starting with ':')
      | Just (0x3A, _) <- BS.uncons line = pure ()  -- 0x3A = ':'
      -- Handle data lines
      | "data: " `BS.isPrefixOf` line = do
          let jsonData = BS.drop 6 line
          -- Check for end of stream
          if jsonData == "[DONE]"
            then pure ()
            else case decode (BL.fromStrict jsonData) of
              Just chunk -> onChunk chunk
              Nothing    -> pure ()  -- Skip malformed chunks
      -- Skip other lines (event:, id:, retry:)
      | otherwise = pure ()
