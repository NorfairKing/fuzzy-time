{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.FuzzyTime.Types.Gen where

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Time ()

import Data.FuzzyTime.Types

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (Some a b)

instance (GenValid a, GenValid b) => GenValid (Some a b) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked AmbiguousLocalTime

instance GenValid AmbiguousLocalTime where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked FuzzyLocalTime

instance GenValid FuzzyLocalTime where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked FuzzyTimeOfDay

instance GenValid FuzzyTimeOfDay where
  genValid =
    (oneof
       [ pure SameTime
       , pure Midnight
       , pure Morning
       , pure Evening
       , AtHour <$> choose (0, 23)
       , AtMinute <$> choose (0, 23) <*> choose (0, 59)
       , AtExact <$> genValid
       , HoursDiff <$> choose (-23, 23)
       , MinutesDiff <$> choose (-(24 * 60 - 1), (24 * 60 - 1))
       , SecondsDiff <$> genValid
       ]) `suchThat`
    isValid
  shrinkValid = shrinkValidStructurally

instance GenUnchecked FuzzyDay

instance GenValid FuzzyDay where
  genValid =
    (oneof
       [ pure Yesterday
       , pure Now
       , pure Today
       , pure Tomorrow
       , OnlyDay <$> choose (1, 31)
       , DayInMonth <$> choose (1, 31) <*> choose (1, 12)
       , DiffDays <$> genValid
       , DiffWeeks <$> genValid
       , NextDayOfTheWeek <$> genValid
       , ExactDay <$> genValid
       ]) `suchThat`
    isValid
  shrinkValid = shrinkValidStructurally

instance GenUnchecked DayOfWeek

instance GenValid DayOfWeek where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked Month

instance GenValid Month where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
