module Data.FuzzyTime.ResolveSpec
    ( spec
    ) where

import Data.Time

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Data.FuzzyTime.Resolve
import Data.FuzzyTime.Types

import Data.FuzzyTime.FuzzyTypes.Gen ()

spec :: Spec
spec =
    describe "resolveDay" $ do
        it "produces valid days" $ producesValidsOnValids2 resolveDay
        it "works for this example for Yesterday" $
            resolveDay (fromGregorian 2000 6 25) Yesterday `shouldBe`
            fromGregorian 2000 6 24
        it "is id for Now" $ forAllValid $ \d -> resolveDay d Now `shouldBe` d
        it "is id for Today" $
            forAllValid $ \d -> resolveDay d Today `shouldBe` d
        it "works for this example for Tomorrow" $
            resolveDay (fromGregorian 2001 6 23) Tomorrow `shouldBe`
            fromGregorian 2001 6 24
        it "produces valid values when given 'OnlyDay' values" $
            forAllValid $ \d ->
                forAllShrink
                    ((OnlyDay <$> genValid) `suchThat` isValid)
                    shrinkValid $ \fd -> shouldBeValid $ resolveDay d fd
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
                    (((\(mi, di) -> DayInMonth mi di) <$> genValid) `suchThat`
                     isValid)
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
        it
            "works for NextDayOfTheWeek with a day of the week after today in the current week" $
            resolveDay (fromGregorian 2018 10 9) (NextDayOfTheWeek Thursday) `shouldBe`
            fromGregorian 2018 10 11
        it
            "works for NextDayOfTheWeek with a day of the week after today in the next week" $
            resolveDay (fromGregorian 2018 10 9) (NextDayOfTheWeek Monday) `shouldBe`
            fromGregorian 2018 10 15
