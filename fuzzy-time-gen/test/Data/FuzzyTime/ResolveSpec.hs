module Data.FuzzyTime.ResolveSpec
  ( spec
  ) where

import Data.Time

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Data.FuzzyTime.Resolve
import Data.FuzzyTime.Types

import Data.FuzzyTime.Types.Gen ()

spec :: Spec
spec = do
  describe "resolveLocalTime" $ do
    it "produces valid local times" $ producesValidsOnValids2 resolveLocalTime
    it "works the same as resolveLocalTimeOne" $
      forAllValid $ \lt ->
        forAllValid $ \fd ->
          resolveLocalTime lt (FuzzyLocalTime (One fd)) `shouldBe`
          OnlyDaySpecified (resolveLocalTimeOne lt fd)
    it "works the same as resolveLocalTimeOther" $
      forAllValid $ \lt ->
        forAllValid $ \ftod ->
          resolveLocalTime lt (FuzzyLocalTime (Other ftod)) `shouldBe`
          BothTimeAndDay (resolveLocalTimeOther lt ftod)
    it "works the same as resolveLocalTimeBoth" $
      forAllValid $ \lt ->
        forAllValid $ \fd ->
          forAllValid $ \ftod ->
            resolveLocalTime lt (FuzzyLocalTime (Both fd ftod)) `shouldBe`
            BothTimeAndDay (resolveLocalTimeBoth lt fd ftod)
    describe "resolveLocalTimeOther" $ do
      it "works for unspecified noon, before noon" $
        forAllValid $ \ld ->
          forAll (genValid `suchThat` (< midday)) $ \tod ->
            resolveLocalTimeOther (LocalTime ld tod) Noon `shouldBe`
            LocalTime ld midday
      it "works for unspecified noon, after noon" $
        forAllValid $ \ld ->
          forAll (genValid `suchThat` (>= midday)) $ \tod ->
            resolveLocalTimeOther (LocalTime ld tod) Noon `shouldBe`
            LocalTime (addDays 1 ld) midday
      it "works for unspecified midnight" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeOther (LocalTime ld tod) Midnight `shouldBe`
            LocalTime (addDays 1 ld) midnight
      it "works for unspecified morning, before morning" $
        forAllValid $ \ld ->
          forAll (genValid `suchThat` (< morning)) $ \tod ->
            resolveLocalTimeOther (LocalTime ld tod) Morning `shouldBe`
            LocalTime ld morning
      it "works for unspecified morning, after morning" $
        forAllValid $ \ld ->
          forAll (genValid `suchThat` (>= morning)) $ \tod ->
            resolveLocalTimeOther (LocalTime ld tod) Morning `shouldBe`
            LocalTime (addDays 1 ld) morning
      it "works for unspecified evening, before evening" $
        forAllValid $ \ld ->
          forAll (genValid `suchThat` (< evening)) $ \tod ->
            resolveLocalTimeOther (LocalTime ld tod) Evening `shouldBe`
            LocalTime ld evening
      it "works for unspecified evening, after evening" $
        forAllValid $ \ld ->
          forAll (genValid `suchThat` (>= evening)) $ \tod ->
            resolveLocalTimeOther (LocalTime ld tod) Evening `shouldBe`
            LocalTime (addDays 1 ld) evening
    describe "resolveLocalTimeBoth" $ do
      describe "SameTime" $ do
        it "works like resolveDay if the fuzzy time of day is SameTime" $
          forAllValid $ \lt@(LocalTime ld tod) ->
            forAllValid $ \fd ->
              resolveLocalTimeBoth lt fd SameTime `shouldBe`
              LocalTime (resolveDay ld fd) tod
      describe "Yesterday" $ do
        it "works without diff" $
          forAllValid $ \lt@(LocalTime ld ltod) ->
            forAllValid $ \ftod ->
              resolveLocalTimeBoth lt Yesterday ftod `shouldBe`
              LocalTime (resolveDay ld Yesterday) (resolveTimeOfDay ltod ftod)
        it "works for noon yesterday" $
          forAllValid $ \ld ->
            forAllValid $ \tod ->
              resolveLocalTimeBoth (LocalTime ld tod) Yesterday Noon `shouldBe`
              LocalTime (addDays (-1) ld) midday
        it "works for midnight yesterday" $
          forAllValid $ \ld ->
            forAllValid $ \tod ->
              resolveLocalTimeBoth (LocalTime ld tod) Yesterday Midnight `shouldBe`
              LocalTime (addDays (-1) ld) midnight
        it "works for morning yesterday" $
          forAllValid $ \ld ->
            forAllValid $ \tod ->
              resolveLocalTimeBoth (LocalTime ld tod) Yesterday Morning `shouldBe`
              LocalTime (addDays (-1) ld) morning
        it "works for evening yesterday" $
          forAllValid $ \ld ->
            forAllValid $ \tod ->
              resolveLocalTimeBoth (LocalTime ld tod) Yesterday Evening `shouldBe`
              LocalTime (addDays (-1) ld) evening
      describe "Tomorrow" $ do
        it "works without diff" $
          forAllValid $ \lt@(LocalTime ld ltod) ->
            forAllValid $ \ftod ->
              resolveLocalTimeBoth lt Tomorrow ftod `shouldBe`
              LocalTime (resolveDay ld Tomorrow) (resolveTimeOfDay ltod ftod)
        it "works for noon tomorrow" $
          forAllValid $ \ld ->
            forAllValid $ \tod ->
              resolveLocalTimeBoth (LocalTime ld tod) Tomorrow Noon `shouldBe`
              LocalTime (addDays 1 ld) midday
        it "works for midnight tomorrow" $
          forAllValid $ \ld ->
            forAllValid $ \tod ->
              resolveLocalTimeBoth (LocalTime ld tod) Tomorrow Midnight `shouldBe`
              LocalTime (addDays 1 ld) midnight
        it "works for morning tomorrow" $
          forAllValid $ \ld ->
            forAllValid $ \tod ->
              resolveLocalTimeBoth (LocalTime ld tod) Tomorrow Morning `shouldBe`
              LocalTime (addDays 1 ld) morning
        it "works for evening tomorrow" $
          forAllValid $ \ld ->
            forAllValid $ \tod ->
              resolveLocalTimeBoth (LocalTime ld tod) Tomorrow Evening `shouldBe`
              LocalTime (addDays 1 ld) evening
  describe "normaliseTimeOfDay" $ do
    it "produces valid times of day" $ producesValid normaliseTimeOfDay
    it "works for this example of tomorrow" $
      normaliseTimeOfDay (TimeOfDay 25 0 0) `shouldBe` (1, TimeOfDay 1 0 0)
    it "works for this example of tomorrow" $
      normaliseTimeOfDay (TimeOfDay 23 120 0) `shouldBe` (1, TimeOfDay 1 0 0)
    it "works for this example of tomorrow" $
      normaliseTimeOfDay (TimeOfDay 23 0 7200) `shouldBe` (1, TimeOfDay 1 0 0)
    it "works for this example of tomorrow" $
      normaliseTimeOfDay (TimeOfDay 23 120 7200) `shouldBe` (1, TimeOfDay 3 0 0)
    it "works for this example of yesterday" $
      normaliseTimeOfDay (TimeOfDay (-1) 0 0) `shouldBe` (-1, TimeOfDay 23 0 0)
    it "works for this example of yesterday" $
      normaliseTimeOfDay (TimeOfDay 0 (-1) 0) `shouldBe` (-1, TimeOfDay 23 59 0)
    it "works for this example of yesterday" $
      normaliseTimeOfDay (TimeOfDay 0 0 (-1)) `shouldBe`
      (-1, TimeOfDay 23 59 59)
    it "works for this example of yesterday" $
      normaliseTimeOfDay (TimeOfDay 0 0 (-0.01)) `shouldBe`
      (-1, TimeOfDay 23 59 59.99)
    it "works for this example of yesterday" $
      normaliseTimeOfDay (TimeOfDay 0 0 (-0.00001)) `shouldBe`
      (-1, TimeOfDay 23 59 59.99999)
    it "works for this example of yesterday" $
      normaliseTimeOfDay (TimeOfDay 0 0 (-0.00000001)) `shouldBe`
      (-1, TimeOfDay 23 59 59.99999999)
  describe "resolveTimeOfDay" $ do
    it "produces valid times of day" $ producesValidsOnValids2 resolveTimeOfDay
    it "works for sametime " $
      forAllValid $ \tod -> resolveTimeOfDay tod SameTime `shouldBe` tod
    it "works for noon " $
      forAllValid $ \tod ->
        resolveTimeOfDay tod Noon `shouldBe` TimeOfDay 12 0 0
    it "works for midnight " $
      forAllValid $ \tod ->
        resolveTimeOfDay tod Midnight `shouldBe` TimeOfDay 0 0 0
    it "works for morning " $
      forAllValid $ \tod ->
        resolveTimeOfDay tod Morning `shouldBe` TimeOfDay 6 0 0
    it "works for evening" $
      forAllValid $ \tod ->
        resolveTimeOfDay tod Evening `shouldBe` TimeOfDay 18 0 0
    it "works for atHour" $
      forAllValid $ \tod ->
        forAllValid $ \h ->
          resolveTimeOfDay tod (AtHour h) `shouldBe` TimeOfDay h 0 0
    it "works for atMinute" $
      forAllValid $ \tod ->
        forAllValid $ \h ->
          forAllValid $ \m ->
            resolveTimeOfDay tod (AtMinute h m) `shouldBe` TimeOfDay h m 0
    it "works for atExact" $
      forAllValid $ \tod1 ->
        forAllValid $ \tod2 ->
          resolveTimeOfDay tod1 (AtExact tod2) `shouldBe` tod2
    it "has an inverse with (small) hoursDiff" $
      forAllValid $ \tod ->
        forAll (choose (-(24 - 1), (24 - 1))) $ \hd ->
          resolveTimeOfDay
            (resolveTimeOfDay tod (HoursDiff hd))
            (HoursDiff (-hd)) `shouldBe`
          tod
    it "has an inverse with (small) minutesDiff" $
      forAllValid $ \tod ->
        forAll (choose (-(24 * 60 - 1), (24 * 60 - 1))) $ \md ->
          resolveTimeOfDay
            (resolveTimeOfDay tod (MinutesDiff md))
            (MinutesDiff (-md)) `shouldBe`
          tod
    it "has an inverse with (small) secondsDiff" $
      forAllValid $ \tod ->
        forAll (max (-1000) . min 1000 <$> genValid) $ \sd ->
          resolveTimeOfDay
            (resolveTimeOfDay tod (SecondsDiff sd))
            (SecondsDiff (-sd)) `shouldBe`
          tod
  describe "resolveDay" $ do
    it "produces valid days" $ producesValidsOnValids2 resolveDay
    it "works for this example for Yesterday" $
      resolveDay (fromGregorian 2000 6 25) Yesterday `shouldBe`
      fromGregorian 2000 6 24
    it "is id for Now" $ forAllValid $ \d -> resolveDay d Now `shouldBe` d
    it "is id for Today" $ forAllValid $ \d -> resolveDay d Today `shouldBe` d
    it "works for this example for Tomorrow" $
      resolveDay (fromGregorian 2001 6 23) Tomorrow `shouldBe`
      fromGregorian 2001 6 24
    it "produces valid values when given 'OnlyDay' values" $
      forAllValid $ \d ->
        forAllShrink ((OnlyDay <$> genValid) `suchThat` isValid) shrinkValid $ \fd ->
          shouldBeValid $ resolveDay d fd
    it
      "works for OnlyDay for this example where the current date is before the given day" $
      resolveDay (fromGregorian 2001 6 23) (OnlyDay 24) `shouldBe`
      fromGregorian 2001 6 24
    it
      "works for OnlyDay for this example where the current date is after the given day" $
      resolveDay (fromGregorian 2001 6 23) (OnlyDay 5) `shouldBe`
      fromGregorian 2001 7 5
    it
      "works for OnlyDay for this example where the following given day is not in this month" $
      resolveDay (fromGregorian 2001 2 23) (OnlyDay 29) `shouldBe`
      fromGregorian 2001 3 29
    it
      "works for OnlyDay for this example where the following given day is not in next month" $
      resolveDay (fromGregorian 2001 1 30) (OnlyDay 29) `shouldBe`
      fromGregorian 2001 3 29
    it
      "works for OnlyDay for this example where the following given day is not in next month" $
      resolveDay (fromGregorian 2001 12 30) (OnlyDay 5) `shouldBe`
      fromGregorian 2002 1 5
    it "produces valid values when given 'DayInMonth' values" $
      forAllValid $ \d ->
        forAllShrink
          (((\(mi, di) -> DayInMonth mi di) <$> genValid) `suchThat` isValid)
          shrinkValid $ \fd -> shouldBeValid $ resolveDay d fd
    it
      "works for DayInMonth for this example where the current date is before the given day" $
      resolveDay (fromGregorian 2001 6 23) (DayInMonth 6 24) `shouldBe`
      fromGregorian 2001 6 24
    it
      "works for DayInMonth for this example where the current date is after the given day" $
      resolveDay (fromGregorian 2001 6 23) (DayInMonth 6 5) `shouldBe`
      fromGregorian 2002 6 5
    it "works for DayInMonth for this example accross years" $
      resolveDay (fromGregorian 2001 1 30) (DayInMonth 1 5) `shouldBe`
      fromGregorian 2002 1 5
    it "works for DayInMonth for this example for february 29th" $
      resolveDay (fromGregorian 2001 1 30) (DayInMonth 2 29) `shouldBe`
      fromGregorian 2004 2 29
    it "produces valid values when given 'DayInMonth' values" $
      forAllValid $ \d ->
        forAllShrink
          ((NextDayOfTheWeek <$> genValid) `suchThat` isValid)
          shrinkValid $ \fd -> shouldBeValid $ resolveDay d fd
    it
      "works for NextDayOfTheWeek with a day of the week after today in the current week" $
      resolveDay (fromGregorian 2018 10 9) (NextDayOfTheWeek Thursday) `shouldBe`
      fromGregorian 2018 10 11
    it
      "works for NextDayOfTheWeek with a day of the week after today in the next week" $
      resolveDay (fromGregorian 2018 10 9) (NextDayOfTheWeek Monday) `shouldBe`
      fromGregorian 2018 10 15
    it
      "works for NextDayOfTheWeek with a day of the week after today in the current week at the end of the year" $
      resolveDay (fromGregorian 2020 12 30) (NextDayOfTheWeek Saturday) `shouldBe`
      fromGregorian 2021 01 02
    it
      "works for NextDayOfTheWeek with a day of the week after today in the next week at the end of the year" $
      resolveDay (fromGregorian 2020 12 30) (NextDayOfTheWeek Tuesday) `shouldBe`
      fromGregorian 2021 01 05
