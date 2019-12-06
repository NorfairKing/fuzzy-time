{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.FuzzyTime.Types.Gen where

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
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked FuzzyDay

instance GenValid FuzzyDay where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked DayOfTheWeek

instance GenValid DayOfTheWeek where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked Month

instance GenValid Month where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
