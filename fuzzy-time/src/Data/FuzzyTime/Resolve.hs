module Data.FuzzyTime.Resolve
    ( resolveZonedTime
    , resolveDay
    ) where

import Data.Maybe
import Data.Time
import Data.Word

import Data.FuzzyTime.Types

resolveZonedTime :: ZonedTime -> FuzzyZonedTime -> ZonedTime
resolveZonedTime zt ZonedNow = zt

resolveDay :: Day -> FuzzyDay -> Day
resolveDay d fd =
    case fd of
        Yesterday -> addDays (-1) d
        Now -> d
        Today -> d
        Tomorrow -> addDays 1 d
        OnlyDay di -> nextDayOnDay d di
        DayInMonth mi di -> nextDayOndayInMonth d mi di
        DiffDays ds -> addDays ds d
        ExactDay d_ -> d_
  where
    nextDayOnDay :: Day -> Int -> Day
    nextDayOnDay d di =
        let (y_, m_, d_) = toGregorian d
            go :: Integer -> [(Month, Int)] -> Day
            go y [] =
                let y' = y + 1
                in go y' (daysInMonth y')
            go y ((month, mds):rest) =
                if mds >= di
                    then if di >= d_
                             then fromGregorian y (monthNum month) di
                             else if y == y_ && month == numMonth m_
                                      then go y rest
                                      else fromGregorian y (monthNum month) di
                    else go y rest
        in go y_ (drop (m_ - 1) $ daysInMonth y_)
    nextDayOndayInMonth :: Day -> Int -> Int -> Day
    nextDayOndayInMonth d mi di =
        let (y_, m_, d_) = toGregorian d
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
