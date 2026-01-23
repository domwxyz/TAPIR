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
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Center (center)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Wrap as Wrap

import Tapir.Types.Response
import Tapir.UI.Types (Name)
import Tapir.UI.Attrs

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
    , padLeft (Pad 2) $ txtWrapWords content
    ]

-- | Simple text wrap for display in viewports
txtWrapWords :: Text -> Widget Name
txtWrapWords t = vBox $ map txt $ T.lines $ Wrap.wrapText Wrap.defaultWrapSettings 80 t

-- ════════════════════════════════════════════════════════════════
-- CONVERSATION MODE
-- ════════════════════════════════════════════════════════════════

renderConversation :: ConversationResponse -> Widget Name
renderConversation ConversationResponse{..} =
  padBottom (Pad 1) $
  vBox $
    -- Main reply
    [ withAttr attrAssistantLabel $ txt "TAPIR"
    , padLeft (Pad 2) $ withAttr attrAssistantMessage $ txtWrapWords convReply
    ]
    -- Inline corrections (if any)
    ++ renderInlineCorrections convCorrections
    -- Vocabulary highlights
    ++ renderVocabSection convVocab
    -- Grammar tip
    ++ renderGrammarTip convGrammarTip
    -- Follow-up suggestion
    ++ renderFollowUp convFollowUp

-- | Render inline corrections (gentle feedback in conversation)
renderInlineCorrections :: Maybe [Correction] -> [Widget Name]
renderInlineCorrections Nothing = []
renderInlineCorrections (Just []) = []
renderInlineCorrections (Just corrs) =
  [ txt " "
  , padLeft (Pad 2) $ vBox
      [ withAttr attrTimestamp $ txt "📝 Small corrections:"
      , padLeft (Pad 2) $ vBox $ map renderMiniCorrection corrs
      ]
  ]
  where
    renderMiniCorrection Correction{..} =
      hBox
        [ withAttr attrError $ txt corrOriginal
        , txt " → "
        , withAttr attrSuccess $ txt corrFixed
        , txt " "
        , withAttr attrTimestamp $ txt $ "(" <> corrExplanation <> ")"
        ]

-- | Render vocabulary highlights section
renderVocabSection :: [VocabHighlight] -> [Widget Name]
renderVocabSection [] = []
renderVocabSection vocab =
  [ txt " "
  , padLeft (Pad 2) $ vBox
      [ withAttr attrTimestamp $ txt "━━━ Vocabulary ━━━"
      , padTop (Pad 1) $ vBox $ map renderVocabItem vocab
      ]
  ]

renderVocabItem :: VocabHighlight -> Widget Name
renderVocabItem VocabHighlight{..} =
  hBox $
    [ withAttr attrCardFront $ txt vhWord
    , txt " = "
    , withAttr attrCardBack $ txt vhTranslation
    ]
    ++ maybe [] (\pos -> [txt $ " (" <> pos <> ")"]) vhPartOfSpeech
    ++ maybe [] (\g -> [withAttr attrTimestamp $ txt $ " [" <> g <> "]"]) vhGender

-- | Render grammar tip
renderGrammarTip :: Maybe Text -> [Widget Name]
renderGrammarTip Nothing = []
renderGrammarTip (Just tip) =
  [ txt " "
  , padLeft (Pad 2) $ hBox
      [ withAttr attrStatusModeActive $ txt "💡 "
      , withAttr attrSystemMessage $ txtWrapWords tip
      ]
  ]

-- | Render follow-up suggestion
renderFollowUp :: Maybe Text -> [Widget Name]
renderFollowUp Nothing = []
renderFollowUp (Just q) =
  [ txt " "
  , padLeft (Pad 2) $ hBox
      [ withAttr attrTimestamp $ txt "Try asking: "
      , withAttr attrPlaceholder $ txt q
      ]
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
  ++ maybe [] (\enc -> [padLeft (Pad 2) $ withAttr attrSystemMessage $ txt enc]) mEnc

-- | Render corrections
renderCorrections :: Text -> Text -> [Correction] -> Maybe Text -> Maybe Text -> [Widget Name]
renderCorrections original corrected corrections mEnc mNote =
  [ withAttr attrTitle $ txt "Original:"
  , padLeft (Pad 2) $ withAttr attrError $ txtWrapWords original
  , txt " "
  , withAttr attrTitle $ txt "Corrected:"
  , padLeft (Pad 2) $ withAttr attrSuccess $ txtWrapWords corrected
  ]
  ++ (if null corrections then [] else
      [ txt " "
      , withAttr attrTitle $ txt "Changes:"
      , padLeft (Pad 2) $ vBox $ map renderCorrectionItem corrections
      ])
  ++ maybe [] (\note ->
      [ txt " "
      , padLeft (Pad 2) $ withAttr attrTimestamp $ txtWrapWords note
      ]) mNote
  ++ maybe [] (\enc ->
      [ txt " "
      , padLeft (Pad 2) $ withAttr attrSystemMessage $ txt enc
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
    , padLeft (Pad 4) $ withAttr attrTimestamp $ txtWrapWords corrExplanation
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
        [ withAttr attrTimestamp $ txt $ trSourceLang <> ": "
        ]
    , padLeft (Pad 2) $ txtWrapWords trSourceText
    , txt " "
    -- Target
    , hBox
        [ withAttr attrTimestamp $ txt $ trTargetLang <> ": "
        , maybe emptyWidget renderFormality trFormality
        ]
    , padLeft (Pad 2) $ withAttr attrSuccess $ txtWrapWords trTargetText
    ]
    -- Literal meaning (if different)
    ++ maybe [] (\lit ->
        [ padLeft (Pad 2) $ hBox
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
  , withAttr attrTitle $ txt "Alternatives:"
  , padLeft (Pad 2) $ vBox $ map (\a -> txt $ "• " <> a) alts
  ]

-- | Render translation notes
renderTranslationNotes :: [TranslationNote] -> [Widget Name]
renderTranslationNotes [] = []
renderTranslationNotes notes =
  [ txt " "
  , withAttr attrTitle $ txt "Notes:"
  , padLeft (Pad 2) $ vBox $ map renderNote notes
  ]
  where
    renderNote TranslationNote{..} =
      vBox $
        [ hBox
            [ withAttr attrModalKey $ txt $ "\"" <> tnPhrase <> "\""
            , txt ": "
            , txtWrapWords tnExplanation
            ]
        ]
        ++ maybe [] (\c -> [padLeft (Pad 2) $ withAttr attrTimestamp $ txt $ "Cultural: " <> c]) tnCultural

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
    , withAttr attrTitle $ txt "Front:"
    , padLeft (Pad 2) $ withAttr attrCardFront $ txt cardRespFront
    , txt " "
    -- Back
    , withAttr attrTitle $ txt "Back:"
    , padLeft (Pad 2) $ withAttr attrCardBack $ txtWrapWords cardRespBack
    ]
    -- Pronunciation
    ++ maybe [] (\p ->
        [ padLeft (Pad 2) $ hBox
            [ withAttr attrTimestamp $ txt "🔊 "
            , txt p
            ]
        ]) cardRespPronunciation
    -- Example
    ++ maybe [] (\ex ->
        [ txt " "
        , withAttr attrTitle $ txt "Example:"
        , padLeft (Pad 2) $ txt ex
        ]
        ++ maybe [] (\trans -> [padLeft (Pad 2) $ withAttr attrTimestamp $ txt $ "(" <> trans <> ")"]) cardRespExampleTrans
        ) cardRespExample
    -- Notes
    ++ maybe [] (\n ->
        [ txt " "
        , withAttr attrTitle $ txt "Notes:"
        , padLeft (Pad 2) $ withAttr attrTimestamp $ txtWrapWords n
        ]) cardRespNotes
    -- Mnemonic
    ++ maybe [] (\m ->
        [ txt " "
        , padLeft (Pad 2) $ hBox
            [ withAttr attrStatusModeActive $ txt "💡 "
            , withAttr attrSystemMessage $ txtWrapWords m
            ]
        ]) cardRespMnemonic
    -- Related words
    ++ (if null cardRespRelated then [] else
        [ txt " "
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
