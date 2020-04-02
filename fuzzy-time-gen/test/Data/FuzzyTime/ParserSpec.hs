{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.FuzzyTime.ParserSpec
  ( spec
  ) where

import Data.GenValidity.Text ()
import Data.Int
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Void
import Text.Printf

import Control.Monad

import Text.Megaparsec

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity

import Data.FuzzyTime
import Data.FuzzyTime.Types.Gen ()

spec :: Spec
spec = do
  describe "fuzzyLocalTimeP" $ do
    parsesValidSpec fuzzyLocalTimeP
    let p = parseJustSpec fuzzyLocalTimeP
        pr = parseJustSpecR fuzzyLocalTimeP
        f = parseNothingSpec fuzzyLocalTimeP
    p "1" (FuzzyLocalTime $ One $ OnlyDay 1)
    p "today" (FuzzyLocalTime $ One Today)
    p "monday" (FuzzyLocalTime $ One $ NextDayOfTheWeek Monday)
    p "8:" (FuzzyLocalTime $ Other $ AtHour 8)
    p "09:" (FuzzyLocalTime $ Other $ AtHour 9)
    p "05:06" (FuzzyLocalTime $ Other $ AtMinute 5 6)
    p "evening" (FuzzyLocalTime $ Other Evening)
    p "tues 05:06" (FuzzyLocalTime $ Both (NextDayOfTheWeek Tuesday) (AtMinute 5 6))
    p "wed 5:06" (FuzzyLocalTime $ Both (NextDayOfTheWeek Wednesday) (AtMinute 5 6))
    p "thu 11:" (FuzzyLocalTime $ Both (NextDayOfTheWeek Thursday) (AtHour 11))
    p "8 05:06" (FuzzyLocalTime $ Both (OnlyDay 8) (AtMinute 5 6))
    p "02-07 05:06" (FuzzyLocalTime $ Both (DayInMonth 2 7) (AtMinute 5 6))
    pr 3 "noon" $ FuzzyLocalTime $ Other Noon
    pr 4 "midday" $ FuzzyLocalTime $ Other Noon
    pr 4 "midnight" $ FuzzyLocalTime $ Other Midnight
    pr 3 "morning" $ FuzzyLocalTime $ Other Morning
    pr 1 "evening" $ FuzzyLocalTime $ Other Evening
    p "6:07" $ FuzzyLocalTime $ Other (AtMinute 6 7)
    p "08:09" $ FuzzyLocalTime $ Other (AtMinute 8 9)
    p "1011" $ FuzzyLocalTime $ Other (AtMinute 10 11)
    p "0324" $ FuzzyLocalTime $ Other (AtMinute 3 24)
    p "23:59:22" $ FuzzyLocalTime $ Other $ AtExact (TimeOfDay 23 59 22)
    p "5:06:23" $ FuzzyLocalTime $ Other $ AtExact (TimeOfDay 5 6 23)
    p "0506:23" $ FuzzyLocalTime $ Other $ AtExact (TimeOfDay 5 6 23)
    p "+5h" $ FuzzyLocalTime $ Other (HoursDiff 5)
    p "-6h" $ FuzzyLocalTime $ Other (HoursDiff (-6))
    p "+7m" $ FuzzyLocalTime $ One (DiffMonths 7)
    p "-8m" $ FuzzyLocalTime $ One (DiffMonths (-8))
    p "+9s" $ FuzzyLocalTime $ Other (SecondsDiff 9)
    p "-10s" $ FuzzyLocalTime $ Other (SecondsDiff (-10))
    f "hello"
    f "world"
  describe "twoDigitsSegmentP" $ do
    parsesValidSpec twoDigitsSegmentP
    let p = parseJustSpec twoDigitsSegmentP
        f = parseNothingSpec twoDigitsSegmentP
    p "0" 0
    p "6" 6
    p "01" 1
    p "12" 12
    p "52" 52
    f "152"
    f "6:"
  describe "hourSegmentP" $ do
    parsesValidSpec hourSegmentP
    let p = parseJustSpec hourSegmentP
        f = parseNothingSpec hourSegmentP
    p "0" 0
    p "6" 6
    p "01" 1
    p "12" 12
    p "7:" 7
    f "25"
    f "52"
    f "152"
  describe "minuteSegmentP" $ do
    parsesValidSpec minuteSegmentP
    let p = parseJustSpec minuteSegmentP
        f = parseNothingSpec minuteSegmentP
    p "0" 0
    p "6" 6
    p "01" 1
    p "12" 12
    p "25" 25
    p "52" 52
    f "152"
    f "8:"
  describe "atHourP" $ do
    parsesValidSpec atHourP
    let p = parseJustSpec atHourP
        f = parseNothingSpec atHourP
    p "0" (AtHour 0)
    p "2" (AtHour 2)
    p "23" (AtHour 23)
    p "08" (AtHour 8)
    p "04" (AtHour 4)
    p "6:" (AtHour 6)
    p "06:" (AtHour 6)
    f "26"
    f "103"
  describe "atMinuteP" $ do
    parsesValidSpec atMinuteP
    let p = parseJustSpec atMinuteP
        f = parseNothingSpec atMinuteP
    p "2:52" (AtMinute 2 52)
    p "23:52" (AtMinute 23 52)
    p "08:15" (AtMinute 08 15)
    p "0426" (AtMinute 4 26)
    f "6:"
    f "06:"
  describe "atExactP" $ do
    parsesValidSpec atExactP
    let p = parseJustSpec atExactP
        f = parseNothingSpec atExactP
    p "23:59:22" $ AtExact (TimeOfDay 23 59 22)
    p "5:06:23" $ AtExact (TimeOfDay 5 6 23)
    p "0506:23" $ AtExact (TimeOfDay 5 6 23)
    f "50623"
    f "050623"
    f "05:0623"
  describe "fuzzyTimeOfDayP" $ do
    parsesValidSpec fuzzyTimeOfDayP
    let p = parseJustSpec fuzzyTimeOfDayP
    let pr = parseJustSpecR fuzzyTimeOfDayP
    pr 2 "noon" Noon
    pr 4 "midday" Noon
    pr 4 "midnight" Midnight
    pr 2 "morning" Morning
    pr 1 "evening" Evening
    describe "AtHour" $ do
      p "0" (AtHour 0)
      p "4" (AtHour 4)
      p "05" (AtHour 5)
      p "6:" (AtHour 6)
      p "07:" (AtHour 7)
    describe "AtMinute" $ do
      p "6:07" (AtMinute 6 7)
      p "08:09" (AtMinute 8 9)
      p "1011" (AtMinute 10 11)
      p "0324" (AtMinute 3 24)
    describe "AtExact" $ do
      p "23:59:22" $ AtExact (TimeOfDay 23 59 22)
      p "5:06:23" $ AtExact (TimeOfDay 5 6 23)
      p "0506:23" $ AtExact (TimeOfDay 5 6 23)
      p "05:06:23.1" $ AtExact (TimeOfDay 5 6 23.1)
      p "05:06:23.01" $ AtExact (TimeOfDay 5 6 23.01)
      p "05:06:23.001" $ AtExact (TimeOfDay 5 6 23.001)
      p "05:06:23.0001" $ AtExact (TimeOfDay 5 6 23.0001)
      p "05:06:23.00001" $ AtExact (TimeOfDay 5 6 23.00001)
      p "05:06:23.000001" $ AtExact (TimeOfDay 5 6 23.000001)
      p "05:06:23.0000001" $ AtExact (TimeOfDay 5 6 23.0000001)
      p "05:06:23.00000001" $ AtExact (TimeOfDay 5 6 23.00000001)
      p "05:06:23.000000001" $ AtExact (TimeOfDay 5 6 23.000000001)
      p "05:06:23.0000000001" $ AtExact (TimeOfDay 5 6 23.0000000001)
      p "05:06:23.00000000001" $ AtExact (TimeOfDay 5 6 23.00000000001)
      p "05:06:23.000000000001" $ AtExact (TimeOfDay 5 6 23.000000000001)
      it "can parse whatever is rendered" $
        forAllValid $ \tod ->
          let s = formatTime defaultTimeLocale "%T%Q" tod
           in case parseForTest fuzzyTimeOfDayP (T.pack s) of
                Left e -> expectationFailure $ errorBundlePretty e
                Right r -> resolveTimeOfDay tod r `shouldBe` tod
    describe "HoursDiff" $ do
      p "+3" (HoursDiff 3)
      p "-4" (HoursDiff (-4))
      p "+5h" (HoursDiff 5)
      p "-6h" (HoursDiff (-6))
    describe "MinutesDiff" $ do
      p "+7m" (MinutesDiff 7)
      p "-8m" (MinutesDiff (-8))
    describe "SecondsDiff" $ do
      p "+9s" (SecondsDiff 9)
      p "-10s" (SecondsDiff (-10))
  describe "fuzzyDayP" $ do
    parsesValidSpec fuzzyDayP
    let fd = parseJustSpecR fuzzyDayP
    fd 1 "yesterday" Yesterday
    fd 3 "today" Today
    fd 3 "tomorrow" Tomorrow
    fd 1 "now" Now
    it "parses exact (recent) days with %Y-%m-%d" $
      forAll (ModifiedJulianDay . toInteger <$> (genValid :: Gen Int16)) $ \day ->
        let t = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" day
         in parseJust fuzzyDayP t $ ExactDay day
    let s = parseJustSpec fuzzyDayP
    let f = parseNothingSpec fuzzyDayP
    it "parses x as OnlyDay x for x between 1 and 31" $
      forAll (choose (1, 31)) $ \i -> parseJust fuzzyDayP (T.pack (show i)) (OnlyDay i)
    s "+3" (DiffDays 3)
    s "-3" (DiffDays $ -3)
    it "Parses +x as DiffDays x" $
      forAllValid $ \i -> parseJust fuzzyDayP (T.pack (printf "%+d" i)) (DiffDays i)
    s "+4d" (DiffDays 4)
    s "-4d" (DiffDays $ -4)
    it "Parses +xd as DiffDays x" $
      forAllValid $ \i -> parseJust fuzzyDayP (T.pack (printf "%+dd" i)) (DiffDays i)
    s "+5w" (DiffWeeks 5)
    s "-5w" (DiffWeeks $ -5)
    it "Parses +xw as DiffWeeks x" $
      forAllValid $ \i -> parseJust fuzzyDayP (T.pack (printf "%+dw" i)) (DiffWeeks i)
    s "+6m" (DiffMonths 6)
    s "-6m" (DiffMonths $ -6)
    it "Parses +xw as DiffMonths x" $
      forAllValid $ \i -> parseJust fuzzyDayP (T.pack (printf "%+dm" i)) (DiffMonths i)
    f "0-0"
    s "2-13" (DayInMonth 2 13)
    s "12-3" (DayInMonth 12 3)
    s "02-13" (DayInMonth 2 13)
    s "12-03" (DayInMonth 12 3)
    s "02-03" (DayInMonth 2 3)
    f "002-03"
    f "02-003"
    f "002-003"
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
               in forAll (elements options) $ \s_ -> parseJust fuzzyDayP s_ (DayInMonth m d)
    it "parses whatever the fuzzy day parser parses, as the next day of the week" $
      forAllValid $ \t ->
        case (,) <$> parse (fuzzyDayOfTheWeekP <* eof) "test input" t <*>
             parse (fuzzyDayP <* eof) "test input" t of
          Left _ -> pure ()
          Right (dow, fd_) ->
            case fd_ of
              NextDayOfTheWeek dow' -> dow' `shouldBe` dow
              _ -> expectationFailure "fuzzyDayP parsed something other than a day of the week"
    it "parses the day of the week string as NextDayOfTheWeek" $
      forAll (elements dayOfTheWeekStrings) $ \(dow, i, t) ->
        forAll (elements $ drop i $ T.inits t) $ \t_ ->
          parseJust fuzzyDayP t_ (NextDayOfTheWeek dow)
  describe "fuzzyDayOfTheWeekP" $ do
    parsesValidSpec fuzzyDayOfTheWeekP
    let fd = parseJustSpecR fuzzyDayOfTheWeekP
    forM_ dayOfTheWeekStrings $ \(dow, ix, s) -> fd ix s dow

dayOfTheWeekStrings :: [(DayOfWeek, Int, Text)]
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
parseJustSpecR p i t res = mapM_ (\s_ -> parseJustSpec p s_ res) $ drop i $ T.inits t

parseJustSpec :: (Show a, Eq a) => Parser a -> Text -> a -> Spec
parseJustSpec p s res = it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parseNothingSpec :: (Show a, Eq a) => Parser a -> Text -> Spec
parseNothingSpec p s = it (unwords ["fails to parse", show s]) $ parseNothing p s

parsesValidSpec :: (Show a, Eq a, Validity a) => Parser a -> Spec
parsesValidSpec p = it "only parses valid values" $ forAllValid $ parsesValid p

parseJust :: (Show a, Eq a) => Parser a -> Text -> a -> Expectation
parseJust p s res =
  case parseForTest p s of
    Right out -> out `shouldBe` res
    Left err ->
      expectationFailure $
      unlines ["Parser failed on input", show s, "with error", errorBundlePretty err]

parseNothing :: (Show a, Eq a) => Parser a -> Text -> Expectation
parseNothing p s =
  case parseForTest p s of
    Left _ -> pure ()
    Right v ->
      expectationFailure $
      unlines
        ["Parser succeeded on input", show s, "at parsing", show v, "but it should have failed."]

parsesValid :: (Show a, Eq a, Validity a) => Parser a -> Text -> Expectation
parsesValid p s =
  case parseForTest p s of
    Left _ -> pure ()
    Right out -> shouldBeValid out

parseForTest :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parseForTest p s = parse (p <* eof) "test input" s
