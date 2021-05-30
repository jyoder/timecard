module Web.Controller.Timecards where

import Data.List (groupBy)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (TimeZone)
import Text.Read (read)
import Web.Controller.Prelude
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

fetchPeopleExcluding ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO [Person]
fetchPeopleExcluding idToExclude = do
    people <- query @Person |> orderByAsc #lastName |> fetch
    filter (\person -> get #id person /= idToExclude) people |> pure

fetchBotId ::
    (?modelContext :: ModelContext) =>
    IO (Id Person)
fetchBotId = get #id <$> fetchBot

fetchBot ::
    (?modelContext :: ModelContext) =>
    IO Person
fetchBot = query @Person |> filterWhere (#goesBy, botName) |> fetchOne

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

botName :: Text
botName = "Tim the Bot"

companyTimeZone :: TimeZone
companyTimeZone = read "PDT"