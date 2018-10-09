{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FuzzyTime.ParserSpec
    ( spec
    ) where

import Data.GenValidity.Text ()
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Text.Printf

import Control.Monad

import Text.Megaparsec

import Test.Hspec
import Test.Validity

import Data.FuzzyTime
import Data.FuzzyTime.FuzzyTypes.Gen ()

spec :: Spec
spec = do
    describe "fuzzyDayP" $ do
        parsesValidSpec fuzzyDayP
        let fd = parseJustSpecR fuzzyDayP
        fd 1 "yesterday" Yesterday
        fd 3 "today" Today
        fd 3 "tomorrow" Tomorrow
        fd 1 "now" Now
        it "parses exact days with %Y-%m-%d" $
            forAllValid $ \day ->
                let t = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" day
                 in parseJust fuzzyDayP t $ ExactDay day
        let s = parseJustSpec fuzzyDayP
        let f = parseNothingSpec fuzzyDayP
        s "0" (DiffDays 0)
        forM_ [1 .. 31] $ \i -> s (T.pack (show i)) (OnlyDay i)
        forM_ [32 .. 50] $ \i -> s (T.pack (show i)) (DiffDays i)
        forM_ [0 .. 50] $ \i -> do
            s (T.pack (printf "%+d" i)) (DiffDays i)
            s (T.pack (printf "%+dd" i)) (DiffDays i)
            s (T.pack (printf "%+d" (negate i))) (DiffDays (negate i))
            s (T.pack (printf "%+dd" (negate i))) (DiffDays (negate i))
        f "0-0"
        forM_ (daysInMonth 2004) $ \(month, mds) -> do
            let m = monthNum month
                optionsFor m_ d_ =
                    nub $ do
                        ms <- [printf "%d" m_, printf "%02d" m_]
                        ds <- [printf "%d" d_, printf "%02d" d_]
                        pure $ T.pack $ concat [ms, "-", ds] :: [Text]
            forM_ [1 .. mds] $ \d -> do
                forM_ (optionsFor m d) $ \t -> s t (DayInMonth m d)
            forM_ [mds + 1 .. 31] $ \d -> forM_ (optionsFor m d) f
    describe "fuzzyDayOfTheWeekP" $ do
        parsesValidSpec fuzzyDayOfTheWeekP
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

parseNothingSpec :: (Show a, Eq a) => Parser a -> Text -> Spec
parseNothingSpec p s =
    it (unwords ["fails to parse", show s]) $ parseNothing p s

parsesValidSpec :: (Show a, Eq a, Validity a) => Parser a -> Spec
parsesValidSpec p = it "only parses valid values" $ forAllValid $ parsesValid p

parseJust :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
parseJust p s res =
    case parse (p <* eof) "test input" s of
        Left err ->
            expectationFailure $
            unlines
                [ "Parser failed on input"
                , show s
                , "with error"
                , parseErrorPretty err
                ]
        Right out -> out `shouldBe` res

parseNothing :: (Show a, Eq a) => Parser a -> Text -> Expectation
parseNothing p s =
    case parse (p <* eof) "test input" s of
        Right v ->
            expectationFailure $
            unlines
                [ "Parser succeeded on input"
                , show s
                , "at parsing"
                , show v
                , "but it should have failed."
                ]
        Left _ -> pure ()

parsesValid :: (Show a, Eq a, Validity a) => Parser a -> Text -> Expectation
parsesValid p s =
    case parse (p <* eof) "test input" s of
        Left _ -> pure ()
        Right out -> shouldBeValid out
