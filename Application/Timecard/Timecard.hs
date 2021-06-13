module Application.Timecard.Timecard where

import Data.Time.Calendar.WeekDate (toWeekDate)
import Generated.Types
import IHP.Prelude

newtype T = T
    { timecardEntries :: [TimecardEntry]
    }

buildAll :: [TimecardEntry] -> [T]
buildAll timecardEntries =
    T . sortEntries <$> groupBy inSameWeek timecardEntries
  where
    sortEntries entries = sortBy dateCompare entries
    dateCompare entryA entryB = get #date entryA `compare` get #date entryB

buildForWeek :: Day -> [TimecardEntry] -> T
buildForWeek date timecardEntries =
    T sortedEntriesInWeek
  where
    sortedEntriesInWeek = sortBy dateCompare entriesInWeek
    entriesInWeek = filter (\timecardEntry -> entryWeek timecardEntry == week) timecardEntries
    week = weekOfYear date
    entryWeek entry = weekOfYear $ get #date entry
    dateCompare entryA entryB = get #date entryA `compare` get #date entryB

inSameWeek :: TimecardEntry -> TimecardEntry -> Bool
inSameWeek timecardEntry1 timecardEntry2 =
    let week1 = weekOfYear $ get #date timecardEntry1
     in let week2 = weekOfYear $ get #date timecardEntry2
         in week1 == week2

weekOfYear :: Day -> (Integer, Int)
weekOfYear day =
    let (year, week, _) = toWeekDate day
     in (year, week)
