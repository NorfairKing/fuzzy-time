{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.FuzzyTime
import Data.FuzzyTime.Types.Gen ()
import Data.GenValidity.Criterion

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @FuzzyLocalTime,
      genValidBench @FuzzyTimeOfDay,
      genValidBench @FuzzyDay,
      genValidBench @DayOfTheWeek,
      genValidBench @Month
    ]
