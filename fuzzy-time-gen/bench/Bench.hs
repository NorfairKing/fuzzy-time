{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion

import Data.GenValidity.Criterion

import Data.FuzzyTime
import Data.FuzzyTime.Types.Gen ()

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @FuzzyLocalTime
    , genValidBench @FuzzyTimeOfDay
    , genValidBench @FuzzyDay
    , genValidBench @DayOfTheWeek
    , genValidBench @Month
    ]
