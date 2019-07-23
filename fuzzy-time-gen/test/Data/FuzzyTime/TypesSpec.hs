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
  eqSpecOnValid @FuzzyDateTime
  genValidSpec @FuzzyDateTime
  eqSpecOnValid @FuzzyTimeOfDay
  genValidSpec @FuzzyTimeOfDay
  eqSpecOnValid @FuzzyDay
  genValidSpec @FuzzyDay
  eqSpecOnValid @DayOfTheWeek
  genValidSpec @DayOfTheWeek
  eqSpecOnValid @Month
  genValidSpec @Month
