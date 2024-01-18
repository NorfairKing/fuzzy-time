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
  genValidSpec @FuzzyLocalTime
  genValidSpec @FuzzyTimeOfDay
  genValidSpec @FuzzyDay
  genValidSpec @DayOfWeek
