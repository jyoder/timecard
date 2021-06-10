module Application.Timecard.Timecard where

import Data.Time.Calendar.WeekDate (toWeekDate)
import Generated.Types
import IHP.Prelude

newtype T = T
    { timecardEntries :: [TimecardEntry]
    }

buildAll :: TimeZone -> [TimecardEntry] -> [T]
buildAll timeZone timecardEntries =
    T . sortEntries <$> groupBy (inSameWeek timeZone) timecardEntries
  where
    sortEntries entries = sortBy dateCompare entries
    dateCompare entryA entryB = get #date entryA `compare` get #date entryB

inSameWeek :: TimeZone -> TimecardEntry -> TimecardEntry -> Bool
inSameWeek timeZone timecardEntry1 timecardEntry2 =
    let week1 = weekOfYear timeZone $ get #date timecardEntry1
     in let week2 = weekOfYear timeZone $ get #date timecardEntry2
         in week1 == week2

weekOfYear :: TimeZone -> UTCTime -> (Integer, Int)
weekOfYear timeZone time =
    let (year, week, _) =
            utcToLocalTime timeZone time
                |> localDay
                |> toWeekDate
     in (year, week)
