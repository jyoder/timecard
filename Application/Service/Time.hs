module Application.Service.Time (
    parseDay,
    startOfWeek,
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