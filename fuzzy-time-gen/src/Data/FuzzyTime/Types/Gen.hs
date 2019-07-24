{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.FuzzyTime.Types.Gen where

import Data.GenValidity
import Data.GenValidity.Time ()

import Data.FuzzyTime.Types

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (Some a b)

instance (GenValid a, GenValid b) => GenValid (Some a b) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked FuzzyLocalTime

instance GenValid FuzzyLocalTime where
  genValid = genValidStructurally

instance GenUnchecked FuzzyTimeOfDay

instance GenValid FuzzyTimeOfDay where
  genValid = genValidStructurally

instance GenUnchecked FuzzyDay

instance GenValid FuzzyDay where
  genValid = genValidStructurally

instance GenUnchecked DayOfTheWeek

instance GenValid DayOfTheWeek where
  genValid = genValidStructurally

instance GenUnchecked Month

instance GenValid Month where
  genValid = genValidStructurally
