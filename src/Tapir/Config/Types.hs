{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tapir.Config.Types
  ( DatabaseConfig(..)
  , UIConfig(..)
  , ChatUIConfig(..)
  , LoggingConfig(..)
  , AppConfig(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

import Tapir.Types.Provider (ProviderConfig)

-- | Database configuration
data DatabaseConfig = DatabaseConfig
  { dbPath :: !FilePath
  , dbAutoBackup :: !Bool
  , dbBackupDir :: !FilePath
  , dbMaxBackups :: !Int
  } deriving (Eq, Show, Generic)

instance FromJSON DatabaseConfig where
  parseJSON = withObject "DatabaseConfig" $ \v ->
    DatabaseConfig
      <$> v .:? "path" .!= "~/.local/share/tapir/tapir.db"
      <*> v .:? "auto_backup" .!= True
      <*> v .:? "backup_dir" .!= "~/.local/share/tapir/backups"
      <*> v .:? "max_backups" .!= 10

instance ToJSON DatabaseConfig

-- | Chat UI configuration
data ChatUIConfig = ChatUIConfig
  { chatShowTimestamps :: !Bool
  , chatTimestampFormat :: !Text
  , chatMaxHistoryLines :: !Int
  , chatWordWrap :: !Bool
  } deriving (Eq, Show, Generic)

instance FromJSON ChatUIConfig where
  parseJSON = withObject "ChatUIConfig" $ \v ->
    ChatUIConfig
      <$> v .:? "show_timestamps" .!= True
      <*> v .:? "timestamp_format" .!= "%H:%M"
      <*> v .:? "max_history_lines" .!= 1000
      <*> v .:? "word_wrap" .!= True

instance ToJSON ChatUIConfig

-- | UI configuration
data UIConfig = UIConfig
  { uiTheme :: !Text
  , uiChat :: !ChatUIConfig
  } deriving (Eq, Show, Generic)

instance FromJSON UIConfig where
  parseJSON = withObject "UIConfig" $ \v ->
    UIConfig
      <$> v .:? "theme" .!= "default"
      <*> v .:? "chat" .!= ChatUIConfig True "%H:%M" 1000 True

instance ToJSON UIConfig

-- | Logging configuration
data LoggingConfig = LoggingConfig
  { logEnabled :: !Bool
  , logLevel :: !Text
  , logFile :: !FilePath
  , logMaxSizeMb :: !Int
  , logRotate :: !Bool
  } deriving (Eq, Show, Generic)

instance FromJSON LoggingConfig where
  parseJSON = withObject "LoggingConfig" $ \v ->
    LoggingConfig
      <$> v .:? "enabled" .!= True
      <*> v .:? "level" .!= "info"
      <*> v .:? "file" .!= "~/.local/share/tapir/tapir.log"
      <*> v .:? "max_size_mb" .!= 10
      <*> v .:? "rotate" .!= True

instance ToJSON LoggingConfig

-- | Main application config
data AppConfig = AppConfig
  { configActiveLanguage :: !Text
  , configProvider :: !ProviderConfig
  , configUI :: !UIConfig
  , configDatabase :: !DatabaseConfig
  , configLogging :: !LoggingConfig
  } deriving (Eq, Show, Generic)

instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \v ->
    AppConfig
      <$> v .:? "active_language" .!= "spanish"
      <*> v .: "provider"
      <*> v .:? "ui" .!= UIConfig "default" (ChatUIConfig True "%H:%M" 1000 True)
      <*> v .:? "database" .!= DatabaseConfig "~/.local/share/tapir/tapir.db" True "~/.local/share/tapir/backups" 10
      <*> v .:? "logging" .!= LoggingConfig True "info" "~/.local/share/tapir/tapir.log" 10 True

instance ToJSON AppConfig
