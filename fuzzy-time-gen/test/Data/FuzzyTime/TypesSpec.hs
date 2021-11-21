{-# LANGUAGE TypeApplications #-}

module Data.FuzzyTime.TypesSpec
  ( spec,
  )
where

import Data.FuzzyTime
import Data.FuzzyTime.Types.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  eqSpec @FuzzyLocalTime
  genValidSpec @FuzzyLocalTime
  eqSpec @FuzzyTimeOfDay
  genValidSpec @FuzzyTimeOfDay
  eqSpec @FuzzyDay
  genValidSpec @FuzzyDay
  eqSpec @DayOfWeek
  genValidSpec @DayOfWeek
  eqSpec @Month
  genValidSpec @Month
