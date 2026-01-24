{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tapir.Types.Provider
  ( ProviderType(..)
  , RateLimit(..)
  , ProviderConfig(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | LLM provider type
data ProviderType
  = OpenRouter
  | Anthropic
  | OpenAI
  | Ollama
  deriving (Eq, Show, Generic)

instance ToJSON ProviderType where
  toJSON = \case
    OpenRouter -> "openrouter"
    Anthropic -> "anthropic"
    OpenAI -> "openai"
    Ollama -> "ollama"

instance FromJSON ProviderType where
  parseJSON = withText "ProviderType" $ \case
    "openrouter" -> pure OpenRouter
    "anthropic" -> pure Anthropic
    "openai" -> pure OpenAI
    "ollama" -> pure Ollama
    _ -> fail "Invalid provider type"

-- | Rate limit configuration
data RateLimit = RateLimit
  { rateLimitRequestsPerMinute :: !Int
  , rateLimitRetryAfterSeconds :: !Int
  } deriving (Eq, Show, Generic)

instance ToJSON RateLimit where
  toJSON rl = object
    [ "requests_per_minute" .= rateLimitRequestsPerMinute rl
    , "retry_after_seconds" .= rateLimitRetryAfterSeconds rl
    ]

instance FromJSON RateLimit where
  parseJSON = withObject "RateLimit" $ \v -> RateLimit
    <$> v .: "requests_per_minute"
    <*> v .: "retry_after_seconds"

-- | Provider configuration
data ProviderConfig = ProviderConfig
  { providerType :: !ProviderType
  , providerApiKeyEnv :: !Text
  , providerApiKey :: !(Maybe Text)
  , providerBaseUrl :: !(Maybe Text)  -- ^ Optional custom base URL (e.g., for Ollama or self-hosted)
  , providerModel :: !Text
  , providerTemperature :: !Double
  , providerMaxTokens :: !Int
  , providerTopP :: !Double
  , providerStream :: !Bool
  , providerRateLimit :: !RateLimit
  , providerTimeoutSeconds :: !Int
  , providerConnectTimeoutSeconds :: !Int
  } deriving (Eq, Show, Generic)

instance FromJSON ProviderConfig where
  parseJSON = withObject "ProviderConfig" $ \v ->
    ProviderConfig
      <$> v .: "type"
      <*> v .:? "api_key_env" .!= "OPENROUTER_API_KEY"
      <*> v .:? "api_key"
      <*> v .:? "base_url"
      <*> v .:? "model" .!= "z-ai/glm-4.7"
      <*> v .:? "temperature" .!= 0.7
      <*> v .:? "max_tokens" .!= 2000
      <*> v .:? "top_p" .!= 1.0
      <*> v .:? "stream" .!= True
      <*> v .:? "rate_limit" .!= RateLimit 20 60
      <*> v .:? "timeout_seconds" .!= 30
      <*> v .:? "connect_timeout_seconds" .!= 10

instance ToJSON ProviderConfig
