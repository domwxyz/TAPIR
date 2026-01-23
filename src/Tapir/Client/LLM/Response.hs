{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.Client.LLM.Response
-- Description : Parse LLM responses into structured types
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.Client.LLM.Response
  ( -- * Response Parsing
    parseResponse
  , parseToolCallResponse
  , extractContent
  
    -- * Utilities
  , ParsedResponse(..)
  , ParseError(..)
  , peMessage
  ) where

import Data.Text (Text)
import Data.Maybe (listToMaybe)

import Tapir.Types (Mode(..))
import Tapir.Types.Response
import Tapir.Client.LLM.Types

-- | Result of parsing an LLM response
data ParsedResponse
  = ParsedStructured !StructuredResponse
    -- ^ Successfully parsed structured response
  | ParsedRawText !Text
    -- ^ Fell back to raw text (no tool call or parse failed)
  | ParsedError !ParseError
    -- ^ Parsing failed completely
  deriving (Eq, Show)

-- | Parse error details
data ParseError = ParseError
  { peMessage :: !Text
  , peRawContent :: !(Maybe Text)
  } deriving (Eq, Show)

-- | Parse a ChatResponse, attempting to extract structured data
parseResponse :: Mode -> ChatResponse -> ParsedResponse
parseResponse mode resp =
  case respChoices resp of
    [] -> ParsedError $ ParseError "No choices in response" Nothing
    (choice:_) ->
      let msg = choiceMessage choice
      in case rmToolCalls msg of
        Just (tc:_) -> parseToolCallResponse mode tc
        _ -> case rmContent msg of
          Just content -> ParsedRawText content
          Nothing -> ParsedError $ ParseError "No content or tool calls" Nothing

-- | Parse a tool call into a structured response
parseToolCallResponse :: Mode -> ToolCall -> ParsedResponse
parseToolCallResponse mode tc =
  let args = fcArguments (tcFunction tc)
      funcName = fcName (tcFunction tc)
  in case parseResponseForMode mode args of
    Right structured -> ParsedStructured structured
    Left err -> ParsedError $ ParseError 
      ("Failed to parse " <> funcName <> " arguments: " <> err)
      (Just args)

-- | Extract raw text content from a response (fallback)
extractContent :: ChatResponse -> Maybe Text
extractContent resp = do
  choice <- listToMaybe (respChoices resp)
  rmContent (choiceMessage choice)
