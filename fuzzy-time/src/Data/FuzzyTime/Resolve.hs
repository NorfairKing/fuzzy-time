module Data.FuzzyTime.Resolve
  ( resolveZonedTime,
    resolveLocalTime,
    resolveLocalTimeOne,
    resolveLocalTimeOther,
    resolveLocalTimeBoth,
    morning,
    evening,
    resolveTimeOfDay,
    resolveTimeOfDayWithDiff,
    normaliseTimeOfDay,
    resolveDay,
  )
where

import Data.Fixed (Pico, mod')
import Data.FuzzyTime.Types (AmbiguousLocalTime (BothTimeAndDay, OnlyDaySpecified), FuzzyDay (DayInMonth, DiffDays, DiffMonths, DiffWeeks, ExactDay, NextDayOfTheWeek, Now, OnlyDay, Today, Tomorrow, Yesterday), FuzzyLocalTime (FuzzyLocalTime), FuzzyTimeOfDay (AtExact, AtHour, AtMinute, Evening, HoursDiff, Midnight, MinutesDiff, Morning, Noon, SameTime, SecondsDiff), FuzzyZonedTime (ZonedNow), Month, Some (Both, One, Other), dayOfTheWeekNum, daysInMonth, monthNum, numMonth)
import Data.Maybe (fromJust)
import Data.Time (Day, DayOfWeek, LocalTime (LocalTime), TimeOfDay (TimeOfDay), ZonedTime, addDays, fromGregorian, midday, midnight, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

resolveZonedTime :: ZonedTime -> FuzzyZonedTime -> ZonedTime
resolveZonedTime zt ZonedNow = zt

resolveLocalTime :: LocalTime -> FuzzyLocalTime -> AmbiguousLocalTime
resolveLocalTime lt (FuzzyLocalTime sft) =
  case sft of
    One fd -> OnlyDaySpecified $ resolveLocalTimeOne lt fd
    Other ftod -> BothTimeAndDay $ resolveLocalTimeOther lt ftod
    Both fd ftod -> BothTimeAndDay $ resolveLocalTimeBoth lt fd ftod

resolveLocalTimeOne :: LocalTime -> FuzzyDay -> Day
resolveLocalTimeOne (LocalTime ld _) fd = resolveDay ld fd

resolveLocalTimeOther :: LocalTime -> FuzzyTimeOfDay -> LocalTime
resolveLocalTimeOther (LocalTime ld ltod) ftod =
  let (d, tod) = resolveTimeOfDayWithDiff ltod ftod
   in LocalTime (addDays d ld) tod

resolveLocalTimeBoth :: LocalTime -> FuzzyDay -> FuzzyTimeOfDay -> LocalTime
resolveLocalTimeBoth (LocalTime ld ltod) fd ftod =
  let withDiff = resolveTimeOfDayWithDiff ltod ftod
      withoutDiff = (0, resolveTimeOfDay ltod ftod)
      (d, tod) =
        case fd of
          Now -> withDiff
          Today -> withDiff
          _ -> withoutDiff
   in LocalTime (addDays d $ resolveDay ld fd) tod

resolveTimeOfDay :: TimeOfDay -> FuzzyTimeOfDay -> TimeOfDay
resolveTimeOfDay tod ftod = snd $ resolveTimeOfDayWithDiff tod ftod

resolveTimeOfDayWithDiff :: TimeOfDay -> FuzzyTimeOfDay -> (Integer, TimeOfDay)
resolveTimeOfDayWithDiff tod@(TimeOfDay h m s) ftod =
  case ftod of
    SameTime -> (0, tod)
    Noon -> next midday
    Midnight -> next midnight
    Morning -> next morning
    Evening -> next evening
    AtHour h_ -> next $ TimeOfDay h_ 0 0
    AtMinute h_ m_ -> next $ TimeOfDay h_ m_ 0
    AtExact tod_ -> next tod_
    HoursDiff hd -> normaliseTimeOfDay (h + fromIntegral hd) m s
    MinutesDiff md -> normaliseTimeOfDay h (m + fromIntegral md) s
    SecondsDiff sd -> normaliseTimeOfDay h m (s + sd)
  where
    next tod_ = (skipIf (>= tod_), tod_)
    skipIf p =
      if p tod
        then 1
        else 0

normaliseTimeOfDay :: Int -> Int -> Pico -> (Integer, TimeOfDay)
normaliseTimeOfDay h m s =
  let s' = s `mod'` 60
      totalM = m + round (s - s') `div` 60
      m' = totalM `mod` 60
      totalH = h + (totalM - m') `div` 60
      h' = totalH `mod` 24
      totalD = (totalH - h') `div` 24
   in (fromIntegral totalD, TimeOfDay h' m' s')

morning :: TimeOfDay
morning = TimeOfDay 6 0 0

evening :: TimeOfDay
evening = TimeOfDay 18 0 0

resolveDay :: Day -> FuzzyDay -> Day
resolveDay d fd =
  case fd of
    Yesterday -> addDays (-1) d
    Now -> d
    Today -> d
    Tomorrow -> addDays 1 d
    OnlyDay di -> nextDayOnDay d di
    DayInMonth mi di -> nextDayOndayInMonth d mi di
    DiffDays ds -> addDays (fromIntegral ds) d
    DiffWeeks ws -> addDays (7 * fromIntegral ws) d
    DiffMonths ms -> addDays (30 * fromIntegral ms) d
    NextDayOfTheWeek dow -> nextDayOfTheWeek d dow
    ExactDay d_ -> d_

nextDayOnDay :: Day -> Int -> Day
nextDayOnDay d di =
  let (y_, m_, _) = toGregorian d
      go :: Integer -> [(Month, Int)] -> Day
      go y [] =
        let y' = y + 1
         in go y' (daysInMonth y')
      go y ((month, mds) : rest) =
        if mds >= di
          then
            let d' = fromGregorian y (monthNum month) di
             in if d' >= d
                  then d'
                  else go y rest
          else go y rest
   in go y_ (drop (m_ - 1) $ daysInMonth y_)

nextDayOndayInMonth :: Day -> Int -> Int -> Day
nextDayOndayInMonth d mi di =
  let (y_, _, _) = toGregorian d
      go y =
        let mds = fromJust $ lookup (numMonth mi) (daysInMonth y)
         in if mds >= di
              then
                let d' = fromGregorian y mi di
                 in if d' >= d
                      then d'
                      else go (y + 1)
              else go (y + 1)
   in go y_

nextDayOfTheWeek :: Day -> DayOfWeek -> Day
nextDayOfTheWeek d dow =
  let (_, _, i_) = toWeekDate d
      down = dayOfTheWeekNum dow
      diff = fromIntegral $ down - i_
      diff' =
        if diff <= 0
          then diff + 7
          else diff
   in addDays diff' d
