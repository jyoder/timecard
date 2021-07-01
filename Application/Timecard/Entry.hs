module Application.Timecard.Entry (
    create,
    update,
    validate,
) where

import Application.Service.Time (startOfWeek)
import Application.Timecard.EntryMessage as Timecard.EntryMessage
import qualified Application.Timecard.Timecard as Timecard
import Generated.Types
import IHP.ControllerPrelude hiding (create)

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
                    Timecard.EntryMessage.createAll (get #id timecardEntry) twilioMessageIds
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
                    Timecard.EntryMessage.replaceAll (get #id timecardEntry) twilioMessageIds
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
