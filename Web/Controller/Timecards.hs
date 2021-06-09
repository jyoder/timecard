module Web.Controller.Timecards where

import Application.Service.People (fetchBotId, fetchPeopleExcluding)
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

        let personActivity = Viewing
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action TimecardEditTimecardEntryAction {..} = do
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId

        selectedTimecardEntry <- fetch timecardEntryId
        let selectedPersonId = get #personId selectedTimecardEntry
        selectedPerson <- fetch selectedPersonId

        timecardEntries <- fetchTimecardEntriesFor selectedPersonId
        let timecards = buildTimecards timecardEntries

        let personActivity = Editing {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action TimecardUpdateTimecardEntryAction = do
        let timecardEntryId = param @(Id TimecardEntry) "id"
        let selectedPersonId = param @(Id Person) "personId"
        let invoiceTranslation = param @Text "invoiceTranslation"

        timecardEntry <- fetch timecardEntryId
        timecardEntry
            |> set #invoiceTranslation invoiceTranslation
            |> validateField #invoiceTranslation nonEmpty
            |> ifValid \case
                Left selectedTimecardEntry -> do
                    botId <- fetchBotId
                    people <- fetchPeopleExcluding botId
                    selectedPerson <- fetch selectedPersonId

                    timecardEntries <- fetchTimecardEntriesFor selectedPersonId
                    let timecards = buildTimecards timecardEntries

                    let personActivity = Editing {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {..}
                Right timecardEntry -> do
                    updateRecord timecardEntry
                    redirectTo TimecardPersonSelectionAction {..}

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