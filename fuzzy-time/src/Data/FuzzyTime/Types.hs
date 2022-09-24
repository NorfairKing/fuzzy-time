{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.FuzzyTime.Types
  ( module Data.FuzzyTime.Types,
    DayOfWeek (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Fixed (Pico)
import Data.Int (Int16)
import Data.Time (Day, DayOfWeek (Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday), LocalTime, TimeOfDay, isLeapYear)
import Data.Validity (Validity (validate), declare, decorate, genericValidate, valid)
import Data.Validity.Time ()
import GHC.Generics (Generic)

data FuzzyZonedTime
  = ZonedNow
  deriving (Show, Eq, Generic)

instance Validity FuzzyZonedTime

instance NFData FuzzyZonedTime

data AmbiguousLocalTime
  = OnlyDaySpecified Day
  | BothTimeAndDay LocalTime
  deriving (Show, Eq, Generic)

instance Validity AmbiguousLocalTime

instance NFData AmbiguousLocalTime

newtype FuzzyLocalTime = FuzzyLocalTime
  { unFuzzyLocalTime :: Some FuzzyDay FuzzyTimeOfDay
  }
  deriving (Show, Eq, Generic)

instance Validity FuzzyLocalTime

instance NFData FuzzyLocalTime

data Some a b
  = One a
  | Other b
  | Both a b
  deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (Some a b)

instance (NFData a, NFData b) => NFData (Some a b)

data FuzzyTimeOfDay
  = SameTime
  | Noon
  | Midnight
  | Morning
  | Evening
  | AtHour Int
  | AtMinute Int Int
  | AtExact TimeOfDay
  | HoursDiff Int -- Max 24
  | MinutesDiff Int -- Max 24 * 60
  | SecondsDiff Pico -- Max 24 * 60 * 60
  deriving (Show, Eq, Generic)

instance Validity FuzzyTimeOfDay where
  validate ftod =
    mconcat
      [ genericValidate ftod,
        case ftod of
          AtHour h ->
            mconcat
              [ declare "The hour is positive" $ h >= 0,
                declare "The hours are fewer than 24" $ h < 24
              ]
          AtMinute h m ->
            mconcat
              [ declare "The hour is positive" $ h >= 0,
                declare "The hours are fewer than 24" $ h < 24,
                declare "The minute is positive" $ m >= 0,
                declare "The minutes are fewer than 60" $ m < 60
              ]
          HoursDiff hs ->
            mconcat
              [declare "The hours difference is no less than 24h" $ abs hs < 24]
          MinutesDiff ms ->
            mconcat
              [ declare "The minutes difference is no less than 1440m" $
                  abs ms < 24 * 60
              ]
          SecondsDiff ms ->
            mconcat
              [ declare "The seconds difference is no less than 86400s" $
                  abs ms < 24 * 60 * 60
              ]
          _ -> valid
      ]

instance NFData FuzzyTimeOfDay

data FuzzyDay
  = Yesterday
  | Now
  | Today
  | Tomorrow
  | OnlyDay Int
  | DayInMonth Int Int
  | DiffDays Int16
  | DiffWeeks Int16
  | DiffMonths Int16
  | NextDayOfTheWeek DayOfWeek
  | ExactDay Day
  deriving (Show, Eq, Generic)

instance Validity FuzzyDay where
  validate fd =
    mconcat
      [ genericValidate fd,
        case fd of
          OnlyDay di ->
            decorate "OnlyDay" $
              mconcat
                [ declare "The day is strictly positive" $ di >= 1,
                  declare "The day is less than or equal to 31" $ di <= 31
                ]
          DayInMonth mi di ->
            decorate "DayInMonth" $
              mconcat
                [ declare "The day is strictly positive" $ di >= 1,
                  declare "The day is less than or equal to 31" $ di <= 31,
                  declare "The month is strictly positive" $ mi >= 1,
                  declare "The month is less than or equal to 12" $ mi <= 12,
                  declare "The number of days makes sense for the month" $
                    maybe False (>= di) $ lookup (numMonth mi) (daysInMonth 2004)
                ]
          _ -> valid
      ]

instance NFData FuzzyDay

deriving instance Generic DayOfWeek

instance NFData DayOfWeek

dayOfTheWeekNum :: DayOfWeek -> Int
dayOfTheWeekNum = fromEnum

numDayOfTheWeek :: Int -> DayOfWeek
numDayOfTheWeek = toEnum

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

instance NFData Month

daysInMonth :: Integer -> [(Month, Int)]
daysInMonth y =
  [ (January, 31),
    ( February,
      if isLeapYear y
        then 29
        else 28
    ),
    (March, 31),
    (April, 30),
    (May, 31),
    (June, 30),
    (July, 31),
    (August, 31),
    (September, 30),
    (October, 31),
    (November, 30),
    (December, 31)
  ]

monthNum :: Month -> Int
monthNum = (+ 1) . fromEnum

numMonth :: Int -> Month
numMonth = toEnum . (\x -> x - 1)
