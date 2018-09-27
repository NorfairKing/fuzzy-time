{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.FuzzyTime.FuzzyTypes.Gen where

import Data.GenValidity
import Data.GenValidity.Time ()

import Data.FuzzyTime.Types

instance GenUnchecked FuzzyDay

instance GenValid FuzzyDay where
    genValid = genValidStructurally

instance GenUnchecked DayOfTheWeek

instance GenValid DayOfTheWeek where
    genValid = genValidStructurally

instance GenUnchecked Month

instance GenValid Month where
    genValid = genValidStructurally
