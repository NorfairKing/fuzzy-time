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
import Test.Hspec.QuickCheck
import Test.QuickCheck
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
        it "parses x as OnlyDay x for x between 1 and 31" $
            forAll (choose (1, 31)) $ \i ->
                parseJust fuzzyDayP (T.pack (show i)) (OnlyDay i)
        it "parses x as DiffDays x for x not 1 and 31" $
            forAll (genUnchecked `suchThat` (\x -> x < 1 || x > 31)) $ \i ->
                parseJust fuzzyDayP (T.pack (show i)) (DiffDays i)
        s "+3" (DiffDays 3)
        s "-3" (DiffDays $ -3)
        it "Parses +x as DiffDays x" $
            forAllUnchecked $ \i ->
                parseJust fuzzyDayP (T.pack (printf "%+d" i)) (DiffDays i)
        s "+3d" (DiffDays 3)
        s "-3d" (DiffDays $ -3)
        it "Parses +xd as DiffDays x" $
            forAllUnchecked $ \i ->
                parseJust fuzzyDayP (T.pack (printf "%+dd" i)) (DiffDays i)
        f "0-0"
        s "2-13" (DayInMonth 2 13)
        s "12-3" (DayInMonth 12 3)
        s "02-13" (DayInMonth 2 13)
        s "12-03" (DayInMonth 12 3)
        s "02-03" (DayInMonth 2 3)
        modifyMaxSuccess (\x -> (x * (365 * 4)) `div` 100) $
            it "parses m-d (in any format) as DayInMonth" $
            forAll (elements $ daysInMonth 2004) $ \(month, mds) ->
                let m = monthNum month
                 in forAll (choose (1, mds)) $ \d ->
                        let options =
                                nub $ do
                                    ms <- [printf "%d" m, printf "%02d" m]
                                    ds <- [printf "%d" d, printf "%02d" d]
                                    pure $ T.pack $ concat [ms, "-", ds] :: [Text]
                         in forAll (elements options) $ \s_ ->
                                parseJust fuzzyDayP s_ (DayInMonth m d)
        it
            "parses whatever the fuzzy day parser parses, as the next day of the week" $
            forAllUnchecked $ \t ->
                case (,) <$> parse (fuzzyDayOfTheWeekP <* eof) "test input" t <*>
                     parse (fuzzyDayP <* eof) "test input" t of
                    Left _ -> pure ()
                    Right (dow, fd_) ->
                        case fd_ of
                            NextDayOfTheWeek dow' -> dow' `shouldBe` dow
                            _ ->
                                expectationFailure
                                    "fuzzyDayP parsed something other than a day of the week"
        it "parses the day of the week string as NextDayOfTheWeek" $
            forAll (elements dayOfTheWeekStrings) $ \(dow, i, t) ->
                forAll (elements $ drop i $ T.inits t) $ \t_ ->
                    parseJust fuzzyDayP t_ (NextDayOfTheWeek dow)
    describe "fuzzyDayOfTheWeekP" $ do
        parsesValidSpec fuzzyDayOfTheWeekP
        let fd = parseJustSpecR fuzzyDayOfTheWeekP
        forM_ dayOfTheWeekStrings $ \(dow, ix, s) -> fd ix s dow

dayOfTheWeekStrings :: [(DayOfTheWeek, Int, Text)]
dayOfTheWeekStrings =
    [ (Monday, 1, "monday")
    , (Tuesday, 2, "tuesday")
    , (Wednesday, 1, "wednesday")
    , (Thursday, 2, "thursday")
    , (Friday, 1, "friday")
    , (Saturday, 2, "saturday")
    , (Sunday, 2, "sunday")
    ]

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
