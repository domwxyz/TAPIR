{-# LANGUAGE OverloadedStrings #-}

module Tapir.Config.Loader
  ( loadConfig
  , loadLanguageModule
  , getConfigDir
  , expandPath
  , PromptVars(..)
  , promptVarsFromModule
  , interpolatePrompt
  , getSystemPrompt
  ) where

import Control.Applicative ((<|>))
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath

import Tapir.Config.Types
import Tapir.Config.Error (ConfigError(..))
import Tapir.Types (Mode, LanguageModule(..))
import Tapir.Types.Language (LanguageInfo(..), LearnerLevel(..), learnerLevelDescription)
import Tapir.Types.Mode (ModeConfig(..), modeToText)
import Tapir.Types.Provider (ProviderConfig(..))

-- | Variables available for prompt interpolation
data PromptVars = PromptVars
  { pvLanguage        :: !Text
  , pvNativeLanguage  :: !Text
  , pvVariant         :: !Text
  , pvLevel           :: !Text
  , pvLevelDesc       :: !Text
  } deriving (Eq, Show)

-- | Get config directory
getConfigDir :: IO FilePath
getConfigDir = do
  home <- getHomeDirectory
  pure $ home </> ".config" </> "tapir"

-- | Load main configuration
loadConfig :: IO (Either ConfigError AppConfig)
loadConfig = do
  configDir <- getConfigDir
  let configPath = configDir </> "config.yaml"

  exists <- doesFileExist configPath
  if not exists
    then pure $ Left $ ConfigNotFound configPath
    else do
      result <- decodeFileEither configPath
      case result of
        Left err  -> pure $ Left $ ConfigParseError configPath (T.pack $ prettyPrintParseException err)
        Right cfg -> expandConfigEnvVars cfg

-- | Expand environment variables in config
expandConfigEnvVars :: AppConfig -> IO (Either ConfigError AppConfig)
expandConfigEnvVars cfg = do
  let prov = configProvider cfg

  -- Expand API key from environment if needed
  apiKey' <- case providerApiKey prov of
    Just k | "${" `T.isPrefixOf` k -> do
      let varName = T.unpack $ T.dropEnd 1 $ T.drop 2 k
      mVal <- lookupEnv varName
      case mVal of
        Just v  -> pure $ Just (T.pack v)
        Nothing -> pure Nothing
    other -> pure other

  -- Also check api_key_env
  apiKey'' <- case apiKey' of
    Nothing -> do
      mVal <- lookupEnv (T.unpack $ providerApiKeyEnv prov)
      pure $ T.pack <$> mVal
    Just k -> pure $ Just k

  pure $ Right $ cfg { configProvider = prov { providerApiKey = apiKey'' } }

-- | Load a language module by name
loadLanguageModule :: Text -> IO (Either ConfigError LanguageModule)
loadLanguageModule langName = do
  configDir <- getConfigDir
  let langPath = configDir </> "languages" </> T.unpack langName <> ".yaml"

  exists <- doesFileExist langPath
  if not exists
    then pure $ Left $ LanguageNotFound langName
    else do
      result <- decodeFileEither langPath
      pure $ case result of
        Left err  -> Left $ ConfigParseError langPath (T.pack $ prettyPrintParseException err)
        Right lm  -> Right lm

-- | Expand ~ in file paths
expandPath :: FilePath -> IO FilePath
expandPath path
  | "~/" `isPrefixOf` path = do
      home <- getHomeDirectory
      pure $ home </> drop 2 path
  | otherwise = pure path

-- | Interpolate variables into a prompt template
interpolatePrompt :: PromptVars -> Text -> Text
interpolatePrompt PromptVars{..} =
    T.replace "{{language}}" pvLanguage
  . T.replace "{{native_language}}" pvNativeLanguage
  . T.replace "{{variant}}" pvVariant
  . T.replace "{{level}}" pvLevel
  . T.replace "{{level_description}}" pvLevelDesc

-- | Convert a config variant token into a human-readable string for prompts
prettyVariant :: Maybe Text -> Text
prettyVariant = \case
  Just "latam"   -> "Latin American"
  Just "spain"   -> "Spain"
  Just "neutral" -> "neutral"
  Just other     -> other
  Nothing        -> "neutral"

-- | Build PromptVars from a LanguageModule
promptVarsFromModule :: LanguageModule -> PromptVars
promptVarsFromModule lm = PromptVars
  { pvLanguage       = languageName (languageInfo lm)
  , pvNativeLanguage = languageNativeLanguage (languageInfo lm)
  , pvVariant        = prettyVariant (languageVariant (languageInfo lm))
  , pvLevel          = T.pack $ show (learnerLevel lm)
  , pvLevelDesc      = learnerLevelDescription (learnerLevel lm)
  }

-- | Get the system prompt for a mode, with variables interpolated
getSystemPrompt :: LanguageModule -> Mode -> Maybe Text
getSystemPrompt lm mode = do
  let modeKey = modeToText mode
  modeConfig <- Map.lookup modeKey (modes lm)
            <|> Map.lookup modeKey (customModes lm)
  let vars = promptVarsFromModule lm
  pure $ interpolatePrompt vars (modeSystemPrompt modeConfig)
