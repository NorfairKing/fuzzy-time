{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.FuzzyTime.TypesSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import Data.FuzzyTime
import Data.FuzzyTime.Types.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @FuzzyLocalTime
  genValidSpec @FuzzyLocalTime
  eqSpecOnValid @FuzzyTimeOfDay
  genValidSpec @FuzzyTimeOfDay
  eqSpecOnValid @FuzzyDay
  genValidSpec @FuzzyDay
  eqSpecOnValid @DayOfWeek
  genValidSpec @DayOfWeek
  eqSpecOnValid @Month
  genValidSpec @Month
