module Application.Service.Time (
    parseDay,
    startOfWeek,
    nextWorkingDay,
) where

import Data.Text (unpack)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import IHP.Prelude

parseDay :: Text -> Maybe Day
parseDay dayString = parseTimeM True defaultTimeLocale "%Y-%m-%d" $ unpack dayString

startOfWeek :: Day -> Day
startOfWeek day =
    let (year, week, _) = toWeekDate day
     in fromWeekDate year week 1

nextWorkingDay :: Day -> Day
nextWorkingDay today =
    case toWeekDate today of
        (_, _, 5) -> addDays 3 today -- Friday we add 3 days to get to Monday
        (_, _, 6) -> addDays 2 today -- Saturday we add 2 days to get to Monday
        _ -> addDays 1 today -- All other days we need only look to tomorrow