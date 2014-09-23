module Data.Time.Extra where

import Prelude
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

-- exclude weekends
-- todo take a list of holidays?
addBusinessDays :: Int -> Day -> Day
addBusinessDays days day = 
  let day' = if isWeekend day then toNextWeek day else day 
      dow  = dayOfWeek day'
      weeks = days `div` 5
      extraWeekendNeeded = (days `mod` 5) > (5 - dow)
      weekends = weeks + (if extraWeekendNeeded then 1 else 0 )
  in addDaysInt (days + 2 * weekends) day'
