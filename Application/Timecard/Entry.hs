module Application.Timecard.Entry (
    create,
    update,
    delete,
    validate,
) where

import qualified Application.Base.AuditEntry as AuditEntry
import Application.Service.Time (startOfWeek)
import Application.Service.Transaction (withTransactionOrSavepoint)
import Application.Timecard.EntryMessage as Timecard.EntryMessage
import qualified Application.Timecard.Timecard as Timecard
import Generated.Types
import IHP.ControllerPrelude hiding (create, delete)

create ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    Id Person ->
    Id PhoneNumber ->
    [Id TwilioMessage] ->
    TimecardEntry ->
    IO (Either TimecardEntry TimecardEntry)
create userId personId phoneNumberId twilioMessageIds timecardEntry =
    withTransactionOrSavepoint do
        timecardEntry <- setTimecardId personId timecardEntry
        validate timecardEntry
            >>= ifValid \case
                Left timecardEntry ->
                    pure $ Left timecardEntry
                Right timecardEntry -> do
                    timecardEntry <- createRecord timecardEntry
                    AuditEntry.createTimecardEntryCreatedEntry userId phoneNumberId timecardEntry
                    Timecard.EntryMessage.createAll (get #id timecardEntry) twilioMessageIds
                    pure $ Right timecardEntry

update ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    Id PhoneNumber ->
    [Id TwilioMessage] ->
    TimecardEntry ->
    IO (Either TimecardEntry TimecardEntry)
update userId phoneNumberId twilioMessageIds timecardEntry = do
    withTransactionOrSavepoint do
        timecard <- fetch (get #timecardId timecardEntry)
        timecardEntry <- setTimecardId (get #personId timecard) timecardEntry
        validate timecardEntry
            >>= ifValid \case
                Left timecardEntry ->
                    pure $ Left timecardEntry
                Right timecardEntry -> do
                    timecardEntry <- updateRecord timecardEntry
                    AuditEntry.createTimecardEntryEditedEntry userId phoneNumberId timecardEntry
                    Timecard.EntryMessage.replaceAll (get #id timecardEntry) twilioMessageIds
                    pure $ Right timecardEntry

delete ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    Id PhoneNumber ->
    Id TimecardEntry ->
    IO ()
delete userId phoneNumberId timecardEntryId =
    withTransactionOrSavepoint do
        Timecard.EntryMessage.deleteAll timecardEntryId
        timecardEntry <- fetch timecardEntryId
        deleteRecord timecardEntry
        AuditEntry.createTimecardEntryDeletedEntry userId phoneNumberId timecardEntry
        pure ()

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
        |> validateField
            #lunchDuration
            ( validateAny [isInList [Nothing, Just 0], isGreaterThan (Just 0)]
                |> withCustomErrorMessage "This field must be greater than or equal to 0"
            )
        |> validateField
            #hoursWorked
            ( validateAny [isInList [0.0], isGreaterThan 0.0]
                |> withCustomErrorMessage "This field must be greater than or equal to 0.0"
            )
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
