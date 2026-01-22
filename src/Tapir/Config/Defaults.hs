{-# LANGUAGE OverloadedStrings #-}

module Tapir.Config.Defaults
  ( defaultConfig
  , defaultProviderConfig
  , defaultUIConfig
  , defaultDatabaseConfig
  , defaultLoggingConfig
  ) where

import Tapir.Config.Types
import Tapir.Types.Provider

-- | Default application config
defaultConfig :: AppConfig
defaultConfig = AppConfig
  { configActiveLanguage = "spanish"
  , configProvider = defaultProviderConfig
  , configUI = defaultUIConfig
  , configDatabase = defaultDatabaseConfig
  , configLogging = defaultLoggingConfig
  }

-- | Default provider (OpenRouter with z-ai/glm-4.7)
defaultProviderConfig :: ProviderConfig
defaultProviderConfig = ProviderConfig
  { providerType = OpenRouter
  , providerApiKeyEnv = "OPENROUTER_API_KEY"
  , providerApiKey = Nothing
  , providerModel = "z-ai/glm-4.7"
  , providerTemperature = 0.7
  , providerMaxTokens = 2000
  , providerTopP = 1.0
  , providerStream = True
  , providerRateLimit = RateLimit 20 60
  , providerTimeoutSeconds = 30
  , providerConnectTimeoutSeconds = 10
  }

-- | Default UI config
defaultUIConfig :: UIConfig
defaultUIConfig = UIConfig
  { uiTheme = "default"
  , uiChat = ChatUIConfig
      { chatShowTimestamps = True
      , chatTimestampFormat = "%H:%M"
      , chatMaxHistoryLines = 1000
      , chatWordWrap = True
      }
  }

-- | Default database config
defaultDatabaseConfig :: DatabaseConfig
defaultDatabaseConfig = DatabaseConfig
  { dbPath = "~/.local/share/tapir/tapir.db"
  , dbAutoBackup = True
  , dbBackupDir = "~/.local/share/tapir/backups"
  , dbMaxBackups = 10
  }

-- | Default logging config
defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig = LoggingConfig
  { logEnabled = True
  , logLevel = "info"
  , logFile = "~/.local/share/tapir/tapir.log"
  , logMaxSizeMb = 10
  , logRotate = True
  }
