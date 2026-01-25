{-# LANGUAGE OverloadedStrings #-}

module Tapir.Service.CardSpec (spec) where

import Test.Hspec
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Tapir.Service.Card
import Tapir.Types (AnkiCard(..))

spec :: Spec
spec = do
  describe "extractCardFromResponse" $ do
    let testTime = posixSecondsToUTCTime 0

    it "parses JSON format" $ do
      let response = "{\"front\": \"hola\", \"back\": \"hello\", \"tags\": [\"greeting\"]}"
      let result = extractCardFromResponse "spanish" "sess1" Nothing response testTime
      fmap cardFront result `shouldBe` Just "hola"
      fmap cardBack result `shouldBe` Just "hello"
      fmap cardTags result `shouldBe` Just ["greeting"]

    it "parses JSON with markdown fences" $ do
      let response = "```json\n{\"front\": \"adios\", \"back\": \"goodbye\"}\n```"
      let result = extractCardFromResponse "spanish" "sess1" Nothing response testTime
      fmap cardFront result `shouldBe` Just "adios"
      fmap cardBack result `shouldBe` Just "goodbye"

    it "parses labeled format" $ do
      let response = "Front: buenos dias\nBack: good morning"
      let result = extractCardFromResponse "spanish" "sess1" Nothing response testTime
      fmap cardFront result `shouldBe` Just "buenos dias"
      fmap cardBack result `shouldBe` Just "good morning"

    it "parses labeled format case-insensitive" $ do
      let response = "front: hola\nback: hello"
      let result = extractCardFromResponse "spanish" "sess1" Nothing response testTime
      fmap cardFront result `shouldBe` Just "hola"
      fmap cardBack result `shouldBe` Just "hello"

    it "parses simple format" $ do
      let response = "gracias\nthank you\nvery common word"
      let result = extractCardFromResponse "spanish" "sess1" Nothing response testTime
      fmap cardFront result `shouldBe` Just "gracias"
      fmap cardBack result `shouldBe` Just "thank you\nvery common word"

    it "returns Nothing for single-line input" $ do
      let response = "just some random text"
      let result = extractCardFromResponse "spanish" "sess1" Nothing response testTime
      result `shouldBe` Nothing

    it "sets correct deck name" $ do
      let response = "{\"front\": \"test\", \"back\": \"test\"}"
      let result = extractCardFromResponse "spanish" "sess1" Nothing response testTime
      fmap cardDeck result `shouldBe` Just "spanish::TAPIR"

    it "uses language as default tag when no tags provided" $ do
      let response = "{\"front\": \"test\", \"back\": \"test\"}"
      let result = extractCardFromResponse "spanish" "sess1" Nothing response testTime
      fmap cardTags result `shouldBe` Just ["spanish"]

  describe "stripMarkdownFences" $ do
    it "removes JSON code fences" $ do
      stripMarkdownFences "```json\n{\"key\": \"value\"}\n```"
        `shouldBe` "{\"key\": \"value\"}\n"

    it "removes plain code fences" $ do
      stripMarkdownFences "```\ncontent here\n```"
        `shouldBe` "content here\n"

    it "handles text without fences" $ do
      stripMarkdownFences "plain text" `shouldBe` "plain text"

    it "handles leading whitespace before fence" $ do
      stripMarkdownFences "\n\n```json\n{}\n```"
        `shouldBe` "{}\n"
