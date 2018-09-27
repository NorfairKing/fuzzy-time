{-# LANGUAGE DeriveGeneric #-}

module Data.FuzzyTime.Types where

import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)

import Data.Time

data FuzzyZonedTime =
    ZonedNow
    deriving (Show, Eq, Generic)

instance Validity FuzzyZonedTime

data FuzzyDay
    = Yesterday
    | Now
    | Today
    | Tomorrow
    | OnlyDay Int -- The day of the month
    | DayInMonth Int -- The month
                 Int -- The day within the month
    | DiffDays Integer -- The difference in days
    | ExactDay Day
    deriving (Show, Eq, Generic)

instance Validity FuzzyDay where
    validate fd =
        mconcat
            [ genericValidate fd
            , case fd of
                  OnlyDay di ->
                      decorate "OnlyDay" $
                      mconcat
                          [ declare "The day is strictly positive" $ di >= 1
                          , declare "The day is less than or equal to 31" $
                            di <= 31
                          ]
                  DayInMonth mi di ->
                      decorate "DayInMonth" $
                      mconcat
                          [ declare "The day is strictly positive" $ di >= 1
                          , declare "The day is less than or equal to 31" $
                            di <= 31
                          , declare "The month is strictly positive" $ mi >= 1
                          , declare "The month is less than or equal to 12" $
                            mi <= 12
                          , declare
                                "The number of days makes sense for the month" $
                            maybe False (>= di) $
                            lookup (numMonth mi) (daysInMonth 2004) -- A leap year
                          ]
                  _ -> valid
            ]

data DayOfTheWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity DayOfTheWeek

data Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity Month

daysInMonth :: Integer -> [(Month, Int)]
daysInMonth y =
    [ (January, 31)
    , ( February
      , if isLeapYear y
            then 29
            else 28)
    , (March, 31)
    , (April, 30)
    , (May, 31)
    , (June, 30)
    , (July, 31)
    , (August, 31)
    , (September, 30)
    , (October, 31)
    , (November, 30)
    , (December, 31)
    ]

monthNum :: Month -> Int
monthNum = (+1) . fromEnum

numMonth :: Int -> Month
numMonth = toEnum . (\x -> x-1)
