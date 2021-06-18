module Application.Timecard.TimecardEntry (
    fetchByPerson,
    fetchByPersonAndWeek,
    create,
    update,
) where

import qualified Application.Timecard.Timecard as Timecard
import qualified Application.Timecard.TimecardEntryMessage as TimecardEntryMessage
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import Database.PostgreSQL.Simple (Query)
import Generated.Types
import IHP.ControllerPrelude hiding (create)
import Text.RawString.QQ (r)

fetchByPerson ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO [TimecardEntry]
fetchByPerson personId =
    query @TimecardEntry
        |> innerJoin @Timecard (#timecardId, #id)
        |> filterWhereJoinedTable @Timecard (#personId, personId)
        |> orderByDesc #date
        |> fetch

fetchByPersonAndWeek ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    Day ->
    IO [TimecardEntry]
fetchByPersonAndWeek person day =
    sqlQuery fetchByPersonAndWeekQuery (person, day)

fetchByPersonAndWeekQuery :: Query
fetchByPersonAndWeekQuery =
    [r|
select
    timecard_entries.*
from
    timecard_entries
where
    timecard_entries.person_id = ?
    and date_trunc('week', timecard_entries.date) = date_trunc('week', (?)::date)
order by
    date desc;
    |]

create ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    [Id TwilioMessage] ->
    TimecardEntry ->
    IO (Either TimecardEntry TimecardEntry)
create personId twilioMessageIds timecardEntry =
    withTransaction do
        timecardEntry <- setTimecardId personId timecardEntry
        validate timecardEntry
            >>= ifValid \case
                Left timecardEntry ->
                    pure $ Left timecardEntry
                Right timecardEntry -> do
                    timecardEntry <- createRecord timecardEntry
                    TimecardEntryMessage.createAll (get #id timecardEntry) twilioMessageIds
                    pure $ Right timecardEntry

update ::
    (?modelContext :: ModelContext) =>
    [Id TwilioMessage] ->
    TimecardEntry ->
    IO (Either TimecardEntry TimecardEntry)
update twilioMessageIds timecardEntry = do
    withTransaction do
        timecard <- fetch (get #timecardId timecardEntry)
        timecardEntry <- setTimecardId (get #personId timecard) timecardEntry
        validate timecardEntry
            >>= ifValid \case
                Left timecardEntry ->
                    pure $ Left timecardEntry
                Right timecardEntry -> do
                    timecardEntry <- updateRecord timecardEntry
                    TimecardEntryMessage.replaceAll (get #id timecardEntry) twilioMessageIds
                    pure $ Right timecardEntry

setTimecardId ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    TimecardEntry ->
    IO TimecardEntry
setTimecardId personId timecardEntry = do
    timecard <- Timecard.fetchOrCreate personId weekOf
    timecardEntry |> set #timecardId (get #id timecard) |> pure
  where
    weekOf = startOfWeek (get #date timecardEntry)

validate ::
    (?modelContext :: ModelContext) =>
    TimecardEntry ->
    IO TimecardEntry
validate timecardEntry =
    timecardEntry
        |> validateField #jobName nonEmpty
        |> validateField #hoursWorked (validateAny [isInList [0.0], isGreaterThan 0.0])
        |> validateField #workDone nonEmpty
        |> validateField #invoiceTranslation nonEmpty
        |> validateFieldIO #date (matchesTimecard (get #timecardId timecardEntry))

matchesTimecard ::
    (?modelContext :: ModelContext) =>
    Id Timecard ->
    Day ->
    IO ValidatorResult
matchesTimecard timecardId date = do
    timecard <- fetch timecardId
    pure
        if startOfWeek date == weekOf timecard
            then Success
            else Failure $ "date must fall within the timecard week " <> show (weekOf timecard)
  where
    weekOf = get #weekOf

startOfWeek :: Day -> Day
startOfWeek day =
    let (year, week, _) = toWeekDate day
     in fromWeekDate year week 1
