module Web.View.Service.Time (
    formatDay,
    formatDayNoYear,
    formatTimeOfDay,
    formatDateTime,
) where

import Data.Text (pack, strip)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format.ISO8601 (iso8601Show)
import Web.View.Prelude

formatDay :: Day -> Text
formatDay = pack . formatTime defaultTimeLocale "%m/%d/%Y"

formatDayNoYear :: Day -> Text
formatDayNoYear = pack . formatTime defaultTimeLocale "%m/%d"

formatTimeOfDay :: TimeOfDay -> Text
formatTimeOfDay timeOfDay =
    strip $ pack $ formatTime defaultTimeLocale "%l:%M %p" timeOfDay

formatDateTime :: UTCTime -> Text
formatDateTime time = strip $ pack $ formatTime defaultTimeLocale "%FT%X%z" time
