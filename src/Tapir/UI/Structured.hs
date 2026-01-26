{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Tapir.UI.Structured
-- Description : Render structured LLM responses in the TUI
-- Copyright   : (c) 2026 Dani Cusanelli
-- License     : MIT

module Tapir.UI.Structured
  ( -- * Main Rendering
    renderStructuredResponse
  
    -- * Mode-Specific Rendering
  , renderConversation
  , renderCorrection
  , renderTranslation
  , renderCardPreview
  ) where

import Brick
import Data.Text (Text)
import qualified Data.Text as T

import Tapir.Types.Response
import Tapir.UI.Types (Name)
import Tapir.UI.Attrs
import Tapir.UI.Widgets (wrapTextDynamic)
import Tapir.Core.Constants (maxVocabHighlightsInChat)

-- ════════════════════════════════════════════════════════════════
-- MAIN DISPATCHER
-- ════════════════════════════════════════════════════════════════

-- | Render any structured response
renderStructuredResponse :: StructuredResponse -> Widget Name
renderStructuredResponse = \case
  SRConversation cr -> renderConversation cr
  SRCorrection cr   -> renderCorrection cr
  SRTranslation tr  -> renderTranslation tr
  SRCard cr         -> renderCardPreview cr
  SRRawText t       -> renderRawText t

-- | Fallback for raw text
renderRawText :: Text -> Widget Name
renderRawText content =
  padBottom (Pad 1) $
  vBox
    [ withAttr attrAssistantLabel $ txt "TAPIR"
    , padLeft (Pad 2) $ wrapTextDynamic content
    ]

-- ════════════════════════════════════════════════════════════════
-- CONVERSATION MODE
-- ════════════════════════════════════════════════════════════════

renderConversation :: ConversationResponse -> Widget Name
renderConversation ConversationResponse{..} =
  padBottom (Pad 1) $
  vBox $
    -- Main reply
    [ withAttr attrAssistantLabel $ txt "TAPIR"
    , padLeft (Pad 2) $ withAttr attrAssistantMessage $ wrapTextDynamic convReply
    ]
    -- Inline corrections (if any) - show prominently when user made mistakes
    ++ renderInlineCorrections convCorrections
    -- Compact vocabulary (1-2 words max)
    ++ renderCompactVocab convVocab
    -- Grammar tip (compact)
    ++ renderGrammarTip convGrammarTip

-- | Render inline corrections, grouped by original→fixed pair
renderInlineCorrections :: [Correction] -> [Widget Name]
renderInlineCorrections [] = []
renderInlineCorrections corrs =
  [txt " "] ++  -- Single blank line
  concatMap renderGroupedCorrection (groupCorrections corrs)
  where
    -- Group corrections by (original, fixed) pair, preserving order
    groupCorrections :: [Correction] -> [((Text, Text), [Text])]
    groupCorrections = foldr insertCorr []
      where
        insertCorr Correction{..} groups =
          let key = (corrOriginal, corrFixed)
          in case findAndUpdate key corrExplanation groups of
            Just updated -> updated
            Nothing -> groups ++ [(key, [corrExplanation])]

        findAndUpdate _ _ [] = Nothing
        findAndUpdate key expl ((k, expls):rest)
          | k == key  = Just ((k, expls ++ [expl]) : rest)
          | otherwise = ((k, expls) :) <$> findAndUpdate key expl rest

    renderGroupedCorrection ((original, fixed), explanations) =
      -- Render original and fixed on separate lines so long text can wrap
      [ padLeft (Pad 2) $ withAttr attrError $ wrapTextDynamic $ "✗ " <> original
      , padLeft (Pad 4) $ hBox
          [ withAttr attrSuccess $ txt "→ "
          , withAttr attrSuccess $ wrapTextDynamic fixed
          ]
      ] ++ map renderExplanation explanations

    renderExplanation expl =
      padLeft (Pad 4) $ withAttr attrTimestamp $ wrapTextDynamic $ "· " <> expl

-- | Render compact vocabulary (each item on its own line for wrapping)
renderCompactVocab :: [VocabHighlight] -> [Widget Name]
renderCompactVocab [] = []
renderCompactVocab vocab =
  let limited = take maxVocabHighlightsInChat vocab
  in [ txt " "  -- Single blank line
     , padLeft (Pad 2) $ withAttr attrTimestamp $ txt "Vocab:"
     ] ++ map renderVocabLine limited
  where
    renderVocabLine VocabHighlight{..} =
      padLeft (Pad 4) $ wrapTextDynamic $ vhWord <> " = " <> vhTranslation

-- | Render grammar tip (with text wrapping)
renderGrammarTip :: Maybe Text -> [Widget Name]
renderGrammarTip Nothing = []
renderGrammarTip (Just tip) =
  [ txt " "
  , padLeft (Pad 2) $ withAttr attrTimestamp $ txt "Tip:"
  , padLeft (Pad 4) $ withAttr attrSystemMessage $ wrapTextDynamic tip
  ]

-- ════════════════════════════════════════════════════════════════
-- CORRECTION MODE
-- ════════════════════════════════════════════════════════════════

renderCorrection :: CorrectionResponse -> Widget Name
renderCorrection CorrectionResponse{..} =
  padBottom (Pad 1) $
  vBox $
    [ withAttr attrAssistantLabel $ txt "CORRECTION"
    , txt " "
    ]
    ++ if crIsCorrect
       then renderPerfectScore crEncouragement
       else renderCorrections crOriginal crCorrected crCorrections crEncouragement crOverallNote

-- | Render when text was perfect
renderPerfectScore :: Maybe Text -> [Widget Name]
renderPerfectScore mEnc =
  [ padLeft (Pad 2) $ withAttr attrSuccess $ txt "✓ Perfect! No corrections needed."
  ]
  ++ maybe [] (\enc -> [txt " ", padLeft (Pad 2) $ withAttr attrSystemMessage $ wrapTextDynamic enc]) mEnc

-- | Render corrections
renderCorrections :: Text -> Text -> [Correction] -> Maybe Text -> Maybe Text -> [Widget Name]
renderCorrections original corrected corrections mEnc mNote =
  [ withAttr attrSectionHeader $ txt "Original:"
  , padLeft (Pad 2) $ withAttr attrError $ wrapTextDynamic original
  , txt " "
  , withAttr attrSectionHeader $ txt "Corrected:"
  , padLeft (Pad 2) $ withAttr attrSuccess $ wrapTextDynamic corrected
  ]
  ++ (if null corrections then [] else
      [ txt " "
      , withAttr attrSectionDivider $ txt "─────────────────"
      , txt " "
      , withAttr attrChangesSection $ txt "Detailed Changes:"
      , txt " "
      , padLeft (Pad 2) $ vBox $ map renderCorrectionItem corrections
      ])
  ++ maybe [] (\note ->
      [ txt " "
      , withAttr attrSectionDivider $ txt "─────────────────"
      , txt " "
      , padLeft (Pad 2) $ withAttr attrTimestamp $ wrapTextDynamic note
      ]) mNote
  ++ maybe [] (\enc ->
      [ txt " "
      , withAttr attrSectionDivider $ txt "─────────────────"
      , txt " "
      , padLeft (Pad 2) $ withAttr attrSystemMessage $ wrapTextDynamic enc
      ]) mEnc

-- | Render a single correction item
renderCorrectionItem :: Correction -> Widget Name
renderCorrectionItem Correction{..} =
  vBox
    [ hBox $
        [ withAttr attrError $ txt corrOriginal
        , txt " → "
        , withAttr attrSuccess $ txt corrFixed
        ]
        ++ maybe [] (\cat -> [txt $ " [" <> categoryText cat <> "]"]) corrCategory
        ++ maybe [] (\sev -> [withAttr (severityAttr sev) $ txt $ " (" <> sev <> ")"]) corrSeverity
    , padLeft (Pad 4) $ withAttr attrTimestamp $ wrapTextDynamic corrExplanation
    ]
  where
    categoryText :: CorrectionCategory -> Text
    categoryText = \case
      CatGrammar     -> "grammar"
      CatSpelling    -> "spelling"
      CatWordChoice  -> "word choice"
      CatPunctuation -> "punctuation"
      CatStyle       -> "style"
      CatGender      -> "gender"
      CatConjugation -> "conjugation"
      CatAccent      -> "accent"
      CatOther       -> "other"
    
    severityAttr :: Text -> AttrName
    severityAttr "minor"       = attrTimestamp
    severityAttr "moderate"    = attrWarning
    severityAttr "significant" = attrError
    severityAttr _             = attrTimestamp

-- ════════════════════════════════════════════════════════════════
-- TRANSLATION MODE
-- ════════════════════════════════════════════════════════════════

renderTranslation :: TranslationResponse -> Widget Name
renderTranslation TranslationResponse{..} =
  padBottom (Pad 1) $
  vBox $
    [ withAttr attrAssistantLabel $ txt "TRANSLATION"
    , txt " "
    -- Source
    , hBox
        [ withAttr attrSectionHeader $ txt $ trSourceLang <> ": "
        ]
    , padLeft (Pad 2) $ wrapTextDynamic trSourceText
    , txt " "
    -- Target
    , hBox
        [ withAttr attrSectionHeader $ txt $ trTargetLang <> ": "
        , maybe emptyWidget renderFormality trFormality
        ]
    , padLeft (Pad 2) $ withAttr attrSuccess $ wrapTextDynamic trTargetText
    ]
    -- Literal meaning (if different)
    ++ maybe [] (\lit ->
        [ txt " "
        , padLeft (Pad 2) $ hBox
            [ withAttr attrTimestamp $ txt "(Literally: "
            , txt lit
            , txt ")"
            ]
        ]) trLiteralMeaning
    -- Alternatives
    ++ renderAlternatives trAlternatives
    -- Notes
    ++ renderTranslationNotes trNotes

-- | Render formality badge
renderFormality :: Formality -> Widget Name
renderFormality f =
  let (label, attr) = case f of
        Formal   -> ("formal", attrStatusLevel)
        Informal -> ("informal", attrStatusModeActive)
        Neutral  -> ("neutral", attrTimestamp)
  in withAttr attr $ txt $ "[" <> label <> "] "

-- | Render alternative translations
renderAlternatives :: [Text] -> [Widget Name]
renderAlternatives [] = []
renderAlternatives alts =
  [ txt " "
  , withAttr attrSectionHeader $ txt "Alternatives:"
  , txt " "
  , padLeft (Pad 2) $ vBox $ map (\a -> wrapTextDynamic $ "• " <> a) alts
  ]

-- | Render translation notes
renderTranslationNotes :: [TranslationNote] -> [Widget Name]
renderTranslationNotes [] = []
renderTranslationNotes notes =
  [ txt " "
  , withAttr attrSectionDivider $ txt "─────────────────"
  , txt " "
  , withAttr attrSectionHeader $ txt "Notes:"
  , txt " "
  , padLeft (Pad 2) $ vBox $ map renderNote notes
  ]
  where
    renderNote TranslationNote{..} =
      vBox $
        [ hBox
            [ withAttr attrModalKey $ txt $ "\"" <> tnPhrase <> "\""
            , txt ": "
            , wrapTextDynamic tnExplanation
            ]
        ]
        ++ maybe [] (\c -> [txt " ", padLeft (Pad 2) $ withAttr attrTimestamp $ wrapTextDynamic $ "Cultural: " <> c]) tnCultural

-- ════════════════════════════════════════════════════════════════
-- CARD GENERATION MODE
-- ════════════════════════════════════════════════════════════════

renderCardPreview :: CardResponse -> Widget Name
renderCardPreview CardResponse{..} =
  padBottom (Pad 1) $
  vBox $
    [ withAttr attrAssistantLabel $ txt "FLASHCARD GENERATED"
    , txt " "
    -- Front
    , withAttr attrSectionHeader $ txt "Front:"
    , padLeft (Pad 2) $ withAttr attrCardFront $ wrapTextDynamic cardRespFront
    , txt " "
    -- Back
    , withAttr attrSectionHeader $ txt "Back:"
    , padLeft (Pad 2) $ withAttr attrCardBack $ wrapTextDynamic cardRespBack
    ]
    -- Pronunciation
    ++ maybe [] (\p ->
        [ txt " "
        , padLeft (Pad 2) $ hBox
            [ withAttr attrTimestamp $ txt "🔊 "
            , wrapTextDynamic p
            ]
        ]) cardRespPronunciation
    -- Example
    ++ maybe [] (\ex ->
        [ txt " "
        , withAttr attrSectionDivider $ txt "─────────────────"
        , txt " "
        , withAttr attrSectionHeader $ txt "Example:"
        , txt " "
        , padLeft (Pad 2) $ wrapTextDynamic ex
        ]
        ++ maybe [] (\trans -> [txt " ", padLeft (Pad 2) $ withAttr attrTimestamp $ wrapTextDynamic $ "(" <> trans <> ")"]) cardRespExampleTrans
        ) cardRespExample
    -- Notes
    ++ maybe [] (\n ->
        [ txt " "
        , withAttr attrSectionHeader $ txt "Notes:"
        , txt " "
        , padLeft (Pad 2) $ withAttr attrTimestamp $ wrapTextDynamic n
        ]) cardRespNotes
    -- Mnemonic
    ++ maybe [] (\m ->
        [ txt " "
        , padLeft (Pad 2) $ hBox
            [ withAttr attrStatusModeActive $ txt "💡 "
            , withAttr attrSystemMessage $ wrapTextDynamic m
            ]
        ]) cardRespMnemonic
    -- Related words
    ++ (if null cardRespRelated then [] else
        [ txt " "
        , withAttr attrSectionDivider $ txt "─────────────────"
        , txt " "
        , hBox
            [ withAttr attrTimestamp $ txt "Related: "
            , txt $ T.intercalate ", " cardRespRelated
            ]
        ])
    -- Tags
    ++ [ txt " "
       , hBox
           [ withAttr attrTimestamp $ txt "Tags: "
           , withAttr attrCardTags $ txt $ T.intercalate ", " cardRespTags
           ]
       ]
