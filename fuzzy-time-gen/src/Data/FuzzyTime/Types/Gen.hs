{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.FuzzyTime.Types.Gen where

import Data.FuzzyTime.Types
import Data.GenValidity
import Data.GenValidity.Time ()
import Test.QuickCheck

instance (GenValid a, GenValid b) => GenValid (Some a b) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AmbiguousLocalTime where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FuzzyLocalTime where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FuzzyTimeOfDay where
  genValid =
    oneof
      [ pure SameTime,
        pure Midnight,
        pure Morning,
        pure Evening,
        AtHour <$> choose (0, 23),
        AtMinute <$> choose (0, 23) <*> choose (0, 59),
        AtExact <$> genValid,
        HoursDiff <$> choose (-23, 23),
        MinutesDiff <$> choose (-(24 * 60 - 1), 24 * 60 - 1),
        SecondsDiff <$> genValid
      ]
      `suchThat` isValid
  shrinkValid = shrinkValidStructurally

instance GenValid FuzzyDay where
  genValid =
    oneof
      [ pure Yesterday,
        pure Now,
        pure Today,
        pure Tomorrow,
        OnlyDay <$> choose (1, 31),
        DayInMonth <$> choose (1, 31) <*> choose (1, 12),
        DiffDays <$> genValid,
        DiffWeeks <$> genValid,
        NextDayOfTheWeek <$> genValid,
        ExactDay <$> genValid
      ]
      `suchThat` isValid
  shrinkValid = shrinkValidStructurally

instance GenValid Month where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
