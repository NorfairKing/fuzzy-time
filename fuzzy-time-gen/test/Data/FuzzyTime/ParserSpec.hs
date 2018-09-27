{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FuzzyTime.ParserSpec
    ( spec
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Text.Megaparsec

import Test.Hspec
import Test.Validity

import Data.FuzzyTime
import Data.FuzzyTime.FuzzyTypes.Gen ()

spec :: Spec
spec = do
    describe "fuzzyDayP" $ do
        let fd = parseJustSpecR fuzzyDayP
        fd 1 "yesterday" Yesterday
        fd 3 "today" Today
        fd 3 "tomorrow" Tomorrow
        fd 1 "now" Now
        it "parses exact days with %Y-%m-%d" $
            forAllValid $ \day ->
                let t = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" day
                in parseJust fuzzyDayP t $ ExactDay day
    describe "fuzzyDayOfTheWeekP" $ do
        let fd = parseJustSpecR fuzzyDayOfTheWeekP
        fd 1 "monday" Monday
        fd 2 "tuesday" Tuesday
        fd 1 "wednesday" Wednesday
        fd 2 "thursday" Thursday
        fd 1 "friday" Friday
        fd 2 "saturday" Saturday
        fd 2 "sunday" Sunday

parseJustSpecR :: (Show a, Eq a) => Parser a -> Int -> Text -> a -> Spec
parseJustSpecR p i t res =
    mapM_ (\s_ -> parseJustSpec p s_ res) $ drop i $ T.inits t

parseJustSpec :: (Show a, Eq a) => Parser a -> Text -> a -> Spec
parseJustSpec p s res =
    it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parseJust :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
parseJust p s res =
    case parse p "test input" s of
        Left err -> expectationFailure $ parseErrorPretty err
        Right out -> out `shouldBe` res
