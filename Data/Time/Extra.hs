{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Time.Extra where

import ClassyPrelude hiding (member)
import Data.Set hiding (map)
import Data.Time
import Data.Time.Calendar.WeekDate

-- module takes monday as first day of the week

addDaysInt :: Int -> Day -> Day
addDaysInt days = addDays (toInteger days)

-- adjusts the given day to the start of the following week
toNextWeek :: Day -> Day
toNextWeek day = addDaysInt (7 - (dayOfWeek day) +1) day

isWeekend :: Day -> Bool
isWeekend = (>5) . dayOfWeek

dayOfWeek :: Day -> Int
dayOfWeek day =
  let (_, _, dow) = toWeekDate day
  in dow

addBusinessDays' :: Int -> Day -> Day
addBusinessDays' days day =
  let day' = if isWeekend day then toNextWeek day else day
      dow  = dayOfWeek day'
      weeks = days `div` 5
      extraWeekendNeeded = (days `mod` 5) > (5 - dow)
      weekends = weeks + (if extraWeekendNeeded then 1 else 0 )
  in addDaysInt (days + 2 * weekends) day'

-- | Simple iterative version which allows holidays to be specified
addBusinessDays :: Set Day -- ^ Set of holidays
                 -> Int     -- ^ Business days to add
                 -> Day     -- ^ Starting day
                 -> Day
addBusinessDays holidays = go
  where
    isHoliday day = member day holidays
    go days day
      | isWeekend day || isHoliday day = go days (addDaysInt 1 day)
      | days <= 0                      = day
      | otherwise                      = go (days - 1) (addDaysInt 1 day)

countWeekDays :: Day -> Day -> Int
countWeekDays now end =
  let days = [ d | d <- [now..end]
                 , not (isWeekend d)
             ]

  in (ClassyPrelude.length days) - 1
