module Data.FuzzyTime.ResolveSpec
  ( spec,
  )
where

import Data.FuzzyTime.Resolve
import Data.FuzzyTime.Types
import Data.FuzzyTime.Types.Gen ()
import Data.Time
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec = do
  describe "resolveLocalTimeForwards" $ do
    it "produces valid local times" $ producesValid2 resolveLocalTimeForwards

    it "works for unspecified noon, before noon" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (< midday)) $ \tod ->
          resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Noon)
            `shouldBe` Just (BothTimeAndDay (LocalTime ld midday))

    it "works for unspecified noon, after noon" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (>= midday)) $ \tod ->
          resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Noon)
            `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) midday))

    it "works for unspecified midnight" $
      forAllValid $ \ld ->
        forAllValid $ \tod ->
          resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Midnight)
            `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) midnight))

    it "works for unspecified morning, before morning" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (< morning)) $ \tod ->
          resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Morning)
            `shouldBe` Just (BothTimeAndDay (LocalTime ld morning))

    it "works for unspecified morning, after morning" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (>= morning)) $ \tod ->
          resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Morning)
            `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) morning))

    it "works for unspecified evening, before evening" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (< evening)) $ \tod ->
          resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Evening)
            `shouldBe` Just (BothTimeAndDay (LocalTime ld evening))

    it "works for unspecified evening, after evening" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (>= evening)) $ \tod ->
          resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Evening)
            `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) evening))

    describe "SameTime" $ do
      it "works like resolveDayForwards if the fuzzy time of day is SameTime" $
        forAllValid $ \lt@(LocalTime ld tod) ->
          forAllValid $ \fd ->
            resolveLocalTimeForwards lt (FuzzyLocalTimeBoth fd SameTime)
              `shouldBe` BothTimeAndDay <$> (LocalTime <$> resolveDayForwards ld fd <*> pure tod)

    describe "Yesterday" $ do
      it "works without diff" $
        forAllValid $ \lt@(LocalTime ld ltod) ->
          forAllValid $ \ftod ->
            resolveLocalTimeForwards lt (FuzzyLocalTimeBoth Yesterday ftod)
              `shouldBe` BothTimeAndDay <$> (LocalTime <$> resolveDayForwards ld Yesterday <*> resolveTimeOfDayForwards ltod ftod)

      it "works for noon yesterday" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeBoth Yesterday Noon)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) midday))

      it "works for midnight yesterday" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeBoth Yesterday Midnight)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) midnight))

      it "works for morning yesterday" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeBoth Yesterday Morning)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) morning))

      it "works for evening yesterday" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeBoth Yesterday Evening)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) evening))

    describe "Tomorrow" $ do
      it "works without diff" $
        forAllValid $ \lt@(LocalTime ld ltod) ->
          forAllValid $ \ftod ->
            resolveLocalTimeForwards lt (FuzzyLocalTimeBoth Tomorrow ftod)
              `shouldBe` BothTimeAndDay <$> (LocalTime <$> resolveDayForwards ld Tomorrow <*> resolveTimeOfDayForwards ltod ftod)

      it "works for noon tomorrow" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeBoth Tomorrow Noon)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) midday))

      it "works for midnight tomorrow" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeBoth Tomorrow Midnight)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) midnight))

      it "works for morning tomorrow" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeBoth Tomorrow Morning)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) morning))

      it "works for evening tomorrow" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeForwards (LocalTime ld tod) (FuzzyLocalTimeBoth Tomorrow Evening)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) evening))

  describe "resolveLocalTimeBackwards" $ do
    it "produces valid local times" $ producesValid2 resolveLocalTimeBackwards

    it "works for unspecified noon, before noon" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (< midday)) $ \tod ->
          resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Noon)
            `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) midday))

    it "works for unspecified noon, after noon" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (>= midday)) $ \tod ->
          resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Noon)
            `shouldBe` Just (BothTimeAndDay (LocalTime ld midday))

    it "works for unspecified midnight" $
      forAllValid $ \ld ->
        forAllValid $ \tod ->
          resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Midnight)
            `shouldBe` Just (BothTimeAndDay (LocalTime ld midnight))

    it "works for unspecified morning, before morning" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (< morning)) $ \tod ->
          resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Morning)
            `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) morning))

    it "works for unspecified morning, after morning" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (>= morning)) $ \tod ->
          resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Morning)
            `shouldBe` Just (BothTimeAndDay (LocalTime ld morning))

    it "works for unspecified evening, before evening" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (< evening)) $ \tod ->
          resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Evening)
            `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) evening))

    it "works for unspecified evening, after evening" $
      forAllValid $ \ld ->
        forAll (genValid `suchThat` (>= evening)) $ \tod ->
          resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeTimeOfDay Evening)
            `shouldBe` Just (BothTimeAndDay (LocalTime ld evening))

    describe "SameTime" $ do
      it "works like resolveDayBackwards if the fuzzy time of day is SameTime" $
        forAllValid $ \lt@(LocalTime ld tod) ->
          forAllValid $ \fd ->
            resolveLocalTimeBackwards lt (FuzzyLocalTimeBoth fd SameTime)
              `shouldBe` BothTimeAndDay <$> (LocalTime <$> resolveDayBackwards ld fd <*> pure tod)

    describe "Yesterday" $ do
      it "works without diff" $
        forAllValid $ \lt@(LocalTime ld ltod) ->
          forAllValid $ \ftod ->
            resolveLocalTimeBackwards lt (FuzzyLocalTimeBoth Yesterday ftod)
              `shouldBe` BothTimeAndDay <$> (LocalTime <$> resolveDayBackwards ld Yesterday <*> resolveTimeOfDayBackwards ltod ftod)

      it "works for noon yesterday" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeBoth Yesterday Noon)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) midday))

      it "works for midnight yesterday" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeBoth Yesterday Midnight)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) midnight))

      it "works for morning yesterday" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeBoth Yesterday Morning)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) morning))

      it "works for evening yesterday" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeBoth Yesterday Evening)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays (-1) ld) evening))

    describe "Tomorrow" $ do
      it "works without diff" $
        forAllValid $ \lt@(LocalTime ld ltod) ->
          forAllValid $ \ftod ->
            resolveLocalTimeBackwards lt (FuzzyLocalTimeBoth Tomorrow ftod)
              `shouldBe` BothTimeAndDay <$> (LocalTime <$> resolveDayBackwards ld Tomorrow <*> resolveTimeOfDayBackwards ltod ftod)

      it "works for noon tomorrow" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeBoth Tomorrow Noon)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) midday))

      it "works for midnight tomorrow" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeBoth Tomorrow Midnight)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) midnight))

      it "works for morning tomorrow" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeBoth Tomorrow Morning)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) morning))

      it "works for evening tomorrow" $
        forAllValid $ \ld ->
          forAllValid $ \tod ->
            resolveLocalTimeBackwards (LocalTime ld tod) (FuzzyLocalTimeBoth Tomorrow Evening)
              `shouldBe` Just (BothTimeAndDay (LocalTime (addDays 1 ld) evening))

  describe "normaliseTimeOfDay" $ do
    it "produces valid times of day" $ producesValid3 normaliseTimeOfDay

    it "works for this example of tomorrow" $
      normaliseTimeOfDay 25 0 0 `shouldBe` (1, TimeOfDay 1 0 0)

    it "works for this example of tomorrow" $
      normaliseTimeOfDay 23 120 0 `shouldBe` (1, TimeOfDay 1 0 0)

    it "works for this example of tomorrow" $
      normaliseTimeOfDay 23 0 7200 `shouldBe` (1, TimeOfDay 1 0 0)

    it "works for this example of tomorrow" $
      normaliseTimeOfDay 23 120 7200 `shouldBe` (1, TimeOfDay 3 0 0)

    it "works for this example of yesterday" $
      normaliseTimeOfDay (-1) 0 0 `shouldBe` (-1, TimeOfDay 23 0 0)

    it "works for this example of yesterday" $
      normaliseTimeOfDay 0 (-1) 0 `shouldBe` (-1, TimeOfDay 23 59 0)

    it "works for this example of yesterday" $
      normaliseTimeOfDay 0 0 (-1) `shouldBe` (-1, TimeOfDay 23 59 59)

    it "works for this example of yesterday" $
      normaliseTimeOfDay 0 0 (-0.01) `shouldBe` (-1, TimeOfDay 23 59 59.99)

    it "works for this example of yesterday" $
      normaliseTimeOfDay 0 0 (-0.00001) `shouldBe` (-1, TimeOfDay 23 59 59.99999)

    it "works for this example of yesterday" $
      normaliseTimeOfDay 0 0 (-0.00000001) `shouldBe` (-1, TimeOfDay 23 59 59.99999999)

    it "does something sensible with leap seconds" $
      normaliseTimeOfDay 23 59 60 `shouldBe` (1, TimeOfDay 0 0 0)

  describe "resolveTimeOfDayForwards" $ do
    it "produces valid times of day" $
      producesValid2 resolveTimeOfDayForwards

    it "works for sametime " $
      forAllValid $ \tod ->
        resolveTimeOfDayForwards tod SameTime `shouldBe` Just tod

    it "works for noon " $
      forAllValid $ \tod ->
        resolveTimeOfDayForwards tod Noon `shouldBe` Just (TimeOfDay 12 0 0)

    it "works for midnight " $
      forAllValid $ \tod ->
        resolveTimeOfDayForwards tod Midnight `shouldBe` Just (TimeOfDay 0 0 0)

    it "works for morning " $
      forAllValid $ \tod ->
        resolveTimeOfDayForwards tod Morning `shouldBe` Just (TimeOfDay 6 0 0)

    it "works for evening" $
      forAllValid $ \tod ->
        resolveTimeOfDayForwards tod Evening `shouldBe` Just (TimeOfDay 18 0 0)

    it "works for atHour" $
      forAllValid $ \tod ->
        forAllValid $ \h ->
          resolveTimeOfDayForwards tod (AtHour h) `shouldBe` Just (TimeOfDay h 0 0)

    it "works for atMinute" $
      forAllValid $ \tod ->
        forAllValid $ \h ->
          forAllValid $ \m ->
            resolveTimeOfDayForwards tod (AtMinute h m) `shouldBe` Just (TimeOfDay h m 0)

    it "works for atExact" $
      forAllValid $ \tod1 ->
        forAllValid $ \tod2 ->
          resolveTimeOfDayForwards tod1 (AtExact tod2) `shouldBe` Just tod2

    xdescribe "Don't actually hold" $ do
      it "has an inverse with (small) hoursDiff" $
        forAllValid $ \tod ->
          forAll (choose (-(24 - 1), 24 - 1)) $ \hd ->
            ( resolveTimeOfDayForwards tod (HoursDiff hd)
                >>= (\tod' -> resolveTimeOfDayForwards tod' (HoursDiff (-hd)))
            )
              `shouldBe` Just tod

      it "has an inverse with (small) minutesDiff" $
        forAllValid $ \tod ->
          forAll (choose (-(24 * 60 - 1), 24 * 60 - 1)) $ \md ->
            ( resolveTimeOfDayForwards tod (MinutesDiff md)
                >>= (\tod' -> resolveTimeOfDayForwards tod' (MinutesDiff (-md)))
            )
              `shouldBe` Just tod

      it "has an inverse with (small) secondsDiff" $
        forAllValid $ \tod ->
          forAll (max (-1000) . min 1000 <$> genValid) $ \sd ->
            ( resolveTimeOfDayForwards tod (SecondsDiff sd)
                >>= (\tod' -> resolveTimeOfDayForwards tod' (SecondsDiff (-sd)))
            )
              `shouldBe` Just tod

  describe "resolveTimeOfDayBackwards" $ do
    it "produces valid times of day" $
      producesValid2 resolveTimeOfDayBackwards

    it "works for sametime " $
      forAllValid $ \tod ->
        resolveTimeOfDayBackwards tod SameTime `shouldBe` Just tod

    it "works for noon " $
      forAllValid $ \tod ->
        resolveTimeOfDayBackwards tod Noon `shouldBe` Just (TimeOfDay 12 0 0)

    it "works for midnight " $
      forAllValid $ \tod ->
        resolveTimeOfDayBackwards tod Midnight `shouldBe` Just (TimeOfDay 0 0 0)

    it "works for morning " $
      forAllValid $ \tod ->
        resolveTimeOfDayBackwards tod Morning `shouldBe` Just (TimeOfDay 6 0 0)

    it "works for evening" $
      forAllValid $ \tod ->
        resolveTimeOfDayBackwards tod Evening `shouldBe` Just (TimeOfDay 18 0 0)

    it "works for atHour" $
      forAllValid $ \tod ->
        forAllValid $ \h ->
          resolveTimeOfDayBackwards tod (AtHour h) `shouldBe` Just (TimeOfDay h 0 0)

    it "works for atMinute" $
      forAllValid $ \tod ->
        forAllValid $ \h ->
          forAllValid $ \m ->
            resolveTimeOfDayBackwards tod (AtMinute h m) `shouldBe` Just (TimeOfDay h m 0)

    it "works for atExact" $
      forAllValid $ \tod1 ->
        forAllValid $ \tod2 ->
          resolveTimeOfDayBackwards tod1 (AtExact tod2) `shouldBe` Just tod2

    xdescribe "Don't actually hold" $ do
      it "has an inverse with (small) hoursDiff" $
        forAllValid $ \tod ->
          forAll (choose (-(24 - 1), 24 - 1)) $ \hd ->
            ( resolveTimeOfDayBackwards tod (HoursDiff hd)
                >>= (\tod' -> resolveTimeOfDayBackwards tod' (HoursDiff (-hd)))
            )
              `shouldBe` Just tod

      it "has an inverse with (small) minutesDiff" $
        forAllValid $ \tod ->
          forAll (choose (-(24 * 60 - 1), 24 * 60 - 1)) $ \md ->
            ( resolveTimeOfDayBackwards tod (MinutesDiff md)
                >>= (\tod' -> resolveTimeOfDayBackwards tod' (MinutesDiff (-md)))
            )
              `shouldBe` Just tod

      it "has an inverse with (small) secondsDiff" $
        forAllValid $ \tod ->
          forAll (max (-1000) . min 1000 <$> genValid) $ \sd ->
            ( resolveTimeOfDayBackwards tod (SecondsDiff sd)
                >>= (\tod' -> resolveTimeOfDayBackwards tod' (SecondsDiff (-sd)))
            )
              `shouldBe` Just tod

  describe "nextDayOfMonth" $ do
    it "produces valid days" $
      producesValid2 nextDayOfMonth

    it "finds next days" $
      forAllValid $ \d ->
        forAllValid $ \di ->
          case nextDayOfMonth d di of
            Nothing -> pure () -- Fine
            Just d' -> d' `shouldSatisfy` (> d)

    it "finds the next 17th of the month in january" $
      nextDayOfMonth (fromGregorian 2023 02 01) 17
        `shouldBe` Just (fromGregorian 2023 02 17)

    it "finds the next 17th of the month in january" $
      nextDayOfMonth (fromGregorian 2023 02 18) 17
        `shouldBe` Just (fromGregorian 2023 03 17)

  describe "previousDayOfMonth" $ do
    it "produces valid days" $
      producesValid2 previousDayOfMonth

    it "finds previous days" $
      forAllValid $ \d ->
        forAllValid $ \di ->
          case previousDayOfMonth d di of
            Nothing -> pure () -- Fine
            Just d' -> d' `shouldSatisfy` (< d)

    it "finds the previous 17th of the month in january" $
      previousDayOfMonth (fromGregorian 2023 02 01) 17
        `shouldBe` Just (fromGregorian 2023 01 17)

    it "finds the previous 17th of the month in january" $
      previousDayOfMonth (fromGregorian 2023 02 18) 17
        `shouldBe` Just (fromGregorian 2023 02 17)

  describe "nextDayOfMonthOfYear" $ do
    it "produces valid days" $
      producesValid3 nextDayOfMonthOfYear

    it "finds next days" $
      forAllValid $ \d ->
        forAllValid $ \w ->
          forAllValid $ \di ->
            case nextDayOfMonthOfYear d w di of
              Nothing -> pure () -- Fine
              Just d' -> d' `shouldSatisfy` (> d)

    it "finds the next 17th of the month in march" $
      nextDayOfMonthOfYear (fromGregorian 2023 02 01) 03 17
        `shouldBe` Just (fromGregorian 2023 03 17)

    it "finds the next 17th of the month in january" $
      nextDayOfMonthOfYear (fromGregorian 2023 02 18) 01 17
        `shouldBe` Just (fromGregorian 2024 01 17)

  describe "previousDayOfMonthOfYear" $ do
    it "produces valid days" $
      producesValid3 previousDayOfMonthOfYear

    it "finds previous days" $
      forAllValid $ \d ->
        forAllValid $ \w ->
          forAllValid $ \di ->
            case previousDayOfMonthOfYear d w di of
              Nothing -> pure () -- Fine
              Just d' -> d' `shouldSatisfy` (< d)

    it "finds the previous 17th of the month in march" $
      previousDayOfMonthOfYear (fromGregorian 2023 02 01) 03 17
        `shouldBe` Just (fromGregorian 2022 03 17)

    it "finds the previous 17th of the month in january" $
      previousDayOfMonthOfYear (fromGregorian 2023 02 18) 01 17
        `shouldBe` Just (fromGregorian 2023 01 17)

  describe "nextDayOfWeek" $ do
    it "produces valid values" $
      producesValid2 nextDayOfWeek

    it "finds next days" $
      forAllValid $ \d ->
        forAllValid $ \dow ->
          nextDayOfWeek d dow `shouldSatisfy` (> d)

    it "finds the day in this week" $
      nextDayOfWeek (fromGregorian 2024 01 18) Friday
        `shouldBe` fromGregorian 2024 01 19

    it "finds the day in next week" $
      nextDayOfWeek (fromGregorian 2024 01 18) Monday
        `shouldBe` fromGregorian 2024 01 22

  describe "previousDayOfWeek" $ do
    it "produces valid values" $
      producesValid2 previousDayOfWeek

    it "finds previous days" $
      forAllValid $ \d ->
        forAllValid $ \dow ->
          previousDayOfWeek d dow `shouldSatisfy` (< d)

    it "finds the day in this week" $
      previousDayOfWeek (fromGregorian 2024 01 18) Friday
        `shouldBe` fromGregorian 2024 01 12

    it "finds the day in next week" $
      previousDayOfWeek (fromGregorian 2024 01 18) Monday
        `shouldBe` fromGregorian 2024 01 15

  describe "resolveDayForwards" $ do
    it "produces valid days" $ producesValid2 resolveDayForwards

    it "works for this example for Yesterday" $
      resolveDayForwards (fromGregorian 2000 6 25) Yesterday
        `shouldBe` Just (fromGregorian 2000 6 24)

    it "is id for Now" $
      forAllValid $ \d ->
        resolveDayForwards d Now `shouldBe` Just d

    it "is id for Today" $
      forAllValid $ \d ->
        resolveDayForwards d Today `shouldBe` Just d

    it "works for this example for Tomorrow" $
      resolveDayForwards (fromGregorian 2001 6 23) Tomorrow
        `shouldBe` Just (fromGregorian 2001 6 24)

    it "produces valid values when given 'OnlyDay' values" $
      forAllValid $ \d ->
        forAllShrink ((OnlyDay <$> genValid) `suchThat` isValid) shrinkValid $ \fd ->
          shouldBeValid $ resolveDayForwards d fd

    it "works for OnlyDay for this example where the current date is before the given day" $
      resolveDayForwards (fromGregorian 2001 6 23) (OnlyDay 24)
        `shouldBe` Just (fromGregorian 2001 6 24)

    it "works for OnlyDay for this example where the current date is after the given day" $
      resolveDayForwards (fromGregorian 2001 6 23) (OnlyDay 5)
        `shouldBe` Just (fromGregorian 2001 7 5)

    it "works for OnlyDay for this example where the following given day is not in this month" $
      resolveDayForwards (fromGregorian 2001 2 23) (OnlyDay 29)
        `shouldBe` Just (fromGregorian 2001 3 29)

    it "does not work for OnlyDay for this example where the following given day is not in next month" $
      resolveDayForwards (fromGregorian 2001 1 30) (OnlyDay 29)
        `shouldBe` Nothing

    it "works for OnlyDay for this example where the following given day is not in next month" $
      resolveDayForwards (fromGregorian 2001 12 30) (OnlyDay 5)
        `shouldBe` Just (fromGregorian 2002 1 5)

    it "produces valid values when given 'DayInMonth' values" $
      forAllValid $ \d ->
        forAllShrink (((\(mi, di) -> DayInMonth (max 0 (min 12 mi)) (max 0 (min 31 di))) <$> genValid) `suchThat` isValid) shrinkValid $ \fd ->
          shouldBeValid $ resolveDayForwards d fd

    it "works for DayInMonth for this example where the current date is before the given day" $
      resolveDayForwards (fromGregorian 2001 6 23) (DayInMonth 6 24)
        `shouldBe` Just (fromGregorian 2001 6 24)

    it "works for DayInMonth for this example where the current date is after the given day" $
      resolveDayForwards (fromGregorian 2001 6 23) (DayInMonth 6 5)
        `shouldBe` Just (fromGregorian 2002 6 5)

    it "works for DayInMonth for this example accross years" $
      resolveDayForwards (fromGregorian 2001 1 30) (DayInMonth 1 5)
        `shouldBe` Just (fromGregorian 2002 1 5)

    it "does not work for DayInMonth for this example for february 29th" $
      resolveDayForwards (fromGregorian 2001 1 30) (DayInMonth 2 29)
        `shouldBe` Nothing

    it "produces valid values when given 'DayOfTheWeek' values" $
      forAllValid $ \d ->
        forAllShrink ((DayOfTheWeek <$> genValid <*> genValid) `suchThat` isValid) shrinkValid $ \fd ->
          shouldBeValid $ resolveDayForwards d fd

    it "works for DayOfTheWeek with a day of the week after today in the current week" $
      resolveDayForwards (fromGregorian 2018 10 9) (DayOfTheWeek Thursday 0)
        `shouldBe` Just (fromGregorian 2018 10 11)

    it "works for DayOfTheWeek with a day of the week after today in the next week" $
      resolveDayForwards (fromGregorian 2018 10 9) (DayOfTheWeek Monday 0)
        `shouldBe` Just (fromGregorian 2018 10 15)
    it
      "works for DayOfTheWeek with a day of the week after today in the current week at the end of the year"
      $ resolveDayForwards (fromGregorian 2020 12 30) (DayOfTheWeek Saturday 0)
        `shouldBe` Just (fromGregorian 2021 01 02)
    it
      "works for DayOfTheWeek with a day of the week after today in the next week at the end of the year"
      $ resolveDayForwards (fromGregorian 2020 12 30) (DayOfTheWeek Tuesday 0)
        `shouldBe` Just (fromGregorian 2021 01 05)

    it "works for DayOfTheWeek with an extra diff" $
      resolveDayForwards (fromGregorian 2018 10 9) (DayOfTheWeek Thursday 2)
        `shouldBe` Just (fromGregorian 2018 10 25)

  describe "resolveDayBackwards" $ do
    it "produces valid days" $ producesValid2 resolveDayBackwards

    it "works for this example for Yesterday" $
      resolveDayBackwards (fromGregorian 2000 6 25) Yesterday
        `shouldBe` Just (fromGregorian 2000 6 24)

    it "is id for Now" $
      forAllValid $ \d ->
        resolveDayBackwards d Now `shouldBe` Just d

    it "is id for Today" $
      forAllValid $ \d ->
        resolveDayBackwards d Today `shouldBe` Just d

    it "works for this example for Tomorrow" $
      resolveDayBackwards (fromGregorian 2001 6 23) Tomorrow
        `shouldBe` Just (fromGregorian 2001 6 24)

    it "produces valid values when given 'OnlyDay' values" $
      forAllValid $ \d ->
        forAllShrink ((OnlyDay <$> genValid) `suchThat` isValid) shrinkValid $ \fd ->
          shouldBeValid $ resolveDayBackwards d fd

    it "works for OnlyDay for this example where the current date is before the given day" $
      resolveDayBackwards (fromGregorian 2001 6 23) (OnlyDay 24)
        `shouldBe` Just (fromGregorian 2001 5 24)

    it "works for OnlyDay for this example where the current date is after the given day" $
      resolveDayBackwards (fromGregorian 2001 6 23) (OnlyDay 5)
        `shouldBe` Just (fromGregorian 2001 6 5)

    it "works for OnlyDay for this example where the following given day is not in this month" $
      resolveDayBackwards (fromGregorian 2001 2 23) (OnlyDay 29)
        `shouldBe` Just (fromGregorian 2001 1 29)

    it "does not work for OnlyDay for this example where the following given day is not in previous month" $
      resolveDayBackwards (fromGregorian 2001 3 25) (OnlyDay 29)
        `shouldBe` Nothing

    it "works for OnlyDay for this example where the following given day is not in previous month" $
      resolveDayBackwards (fromGregorian 2002 01 02) (OnlyDay 5)
        `shouldBe` Just (fromGregorian 2001 12 5)

    it "produces valid values when given 'DayInMonth' values" $
      forAllValid $ \d ->
        forAllShrink (((\(mi, di) -> DayInMonth (max 0 (min 12 mi)) (max 0 (min 31 di))) <$> genValid) `suchThat` isValid) shrinkValid $ \fd ->
          shouldBeValid $ resolveDayBackwards d fd

    it "works for DayInMonth for this example where the current date is before the given day" $
      resolveDayBackwards (fromGregorian 2001 6 23) (DayInMonth 6 24)
        `shouldBe` Just (fromGregorian 2000 6 24)

    it "works for DayInMonth for this example where the current date is after the given day" $
      resolveDayBackwards (fromGregorian 2001 6 23) (DayInMonth 6 5)
        `shouldBe` Just (fromGregorian 2001 6 5)

    it "works for DayInMonth for this example accross years" $
      resolveDayBackwards (fromGregorian 2002 1 03) (DayInMonth 1 5)
        `shouldBe` Just (fromGregorian 2001 1 5)

    it "does not work for DayInMonth for this example for february 29th" $
      resolveDayBackwards (fromGregorian 2002 3 20) (DayInMonth 2 29)
        `shouldBe` Nothing

    it "produces valid values when given 'DayInMonth' values" $
      forAllValid $ \d ->
        forAllShrink ((DayOfTheWeek <$> genValid <*> genValid) `suchThat` isValid) shrinkValid $ \fd ->
          shouldBeValid $ resolveDayBackwards d fd

    it "works for DayOfTheWeek with a day of the week after today in the current week" $
      resolveDayBackwards (fromGregorian 2018 10 9) (DayOfTheWeek Thursday 0)
        `shouldBe` Just (fromGregorian 2018 10 04)

    it "works for DayOfTheWeek with a day of the week after today in the previous week" $
      resolveDayBackwards (fromGregorian 2018 10 9) (DayOfTheWeek Monday 0)
        `shouldBe` Just (fromGregorian 2018 10 08)
    it
      "works for DayOfTheWeek with a day of the week after today in the current week at the end of the year"
      $ resolveDayBackwards (fromGregorian 2021 01 02) (DayOfTheWeek Saturday 0)
        `shouldBe` Just (fromGregorian 2020 12 26)
    it
      "works for DayOfTheWeek with a day of the week after today in the previous week at the end of the year"
      $ resolveDayBackwards (fromGregorian 2021 01 01) (DayOfTheWeek Tuesday 0)
        `shouldBe` Just (fromGregorian 2020 12 29)

    it "works for DayOfTheWeek with a day of the week after today in the current week" $
      resolveDayBackwards (fromGregorian 2018 10 9) (DayOfTheWeek Thursday (-2))
        `shouldBe` Just (fromGregorian 2018 09 20)
