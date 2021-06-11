module Web.View.Service.Time where

import Data.Text (pack)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format.ISO8601 (iso8601Show)
import IHP.View.TimeAgo as TO
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.View.Prelude

formatDay :: Day -> Text
formatDay = pack . formatTime defaultTimeLocale "%m/%d/%Y"

weekday :: UTCTime -> Html
weekday = timeElement "weekday"

timeElement :: Text -> UTCTime -> Html
timeElement className dateTime =
    H.time
        ! A.class_ (cs className)
        ! A.datetime (cs $ iso8601Show dateTime)
        $ cs (beautifyUtcTime dateTime)

beautifyUtcTime :: UTCTime -> String
beautifyUtcTime = formatTime defaultTimeLocale "%d.%m.%Y, %H:%M"