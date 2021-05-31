module Web.Controller.Timecards where

import Data.List (groupBy)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (TimeZone)
import Text.Read (read)
import Web.Controller.Prelude
import Web.Controller.Service.People (fetchBotId, fetchPeopleExcluding)
import Web.View.Timecards.Index

instance Controller TimecardsController where
    beforeAction = ensureIsUser

    action TimecardsAction = do
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        let personSelection = NoPersonSelected

        render IndexView {..}
    --
    action TimecardPersonSelectionAction {..} = do
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        selectedPerson <- fetch selectedPersonId

        timecardEntries <- fetchTimecardEntriesFor selectedPersonId
        let timecards = buildTimecards timecardEntries

        let personSelection = PersonSelected {..}

        render IndexView {..}

fetchTimecardEntriesFor ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO [TimecardEntry]
fetchTimecardEntriesFor personId =
    query @TimecardEntry
        |> filterWhere (#personId, personId)
        |> orderByDesc #date
        |> fetch

buildTimecards :: [TimecardEntry] -> [Timecard]
buildTimecards timecardEntries =
    Timecard . sortEntries <$> groupBy inSameWeek timecardEntries
  where
    sortEntries entries = sortBy dateCompare entries
    dateCompare entryA entryB = get #date entryA `compare` get #date entryB

inSameWeek :: TimecardEntry -> TimecardEntry -> Bool
inSameWeek timecardEntry1 timecardEntry2 =
    let week1 = weekOfYear companyTimeZone $ get #date timecardEntry1
     in let week2 = weekOfYear companyTimeZone $ get #date timecardEntry2
         in week1 == week2

weekOfYear :: TimeZone -> UTCTime -> (Integer, Int)
weekOfYear timeZone time =
    let (year, week, _) =
            utcToZonedTime companyTimeZone time
                |> zonedTimeToLocalTime
                |> localDay
                |> toWeekDate
     in (year, week)

companyTimeZone :: TimeZone
companyTimeZone = read "PDT"