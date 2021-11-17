module Application.Timecard.Entry (
    create,
    update,
    delete,
    validate,
    isBefore,
    isAfter,
    setClockedInAt,
    setClockedOutAt,
    matchesClockDetails,
    clockDetailsMatchHoursWorked,
    clockDetailsToHoursWorked,
) where

import qualified Application.Audit.Entry as Audit.Entry
import qualified Application.Brain.Normalize as Brain.Normalize
import Application.Service.Text (isBlank)
import Application.Service.Time (startOfWeek)
import Application.Service.Transaction (withTransactionOrSavepoint)
import Application.Timecard.EntryMessage as Timecard.EntryMessage
import qualified Application.Timecard.Timecard as Timecard
import Data.Time.Clock (diffTimeToPicoseconds)
import Data.Time.LocalTime (timeOfDayToTime)
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
                    Audit.Entry.createTimecardEntryCreated userId phoneNumberId timecardEntry
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
                    Audit.Entry.createTimecardEntryEdited userId phoneNumberId timecardEntry
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
        Audit.Entry.createTimecardEntryDeleted userId phoneNumberId timecardEntry
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
            #clockedInAt
            ( isBefore (get #clockedOutAt timecardEntry)
                |> withCustomErrorMessage "Must be earlier than clock out time"
            )
        |> validateField
            #clockedOutAt
            ( isAfter (get #clockedInAt timecardEntry)
                |> withCustomErrorMessage "Must be later than clock in time"
            )
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
        |> validateField #hoursWorked (matchesClockDetails (get #clockedInAt timecardEntry) (get #clockedOutAt timecardEntry) (get #lunchDuration timecardEntry))
        |> validateField #workDone nonEmpty
        |> validateField #invoiceTranslation nonEmpty
        |> validateFieldIO #date (matchesTimecard (get #timecardId timecardEntry))

isBefore :: Maybe TimeOfDay -> Maybe TimeOfDay -> ValidatorResult
isBefore maybeEndTime maybeStartTime =
    case (maybeStartTime, maybeEndTime) of
        (Just startTime, Just endTime) ->
            if startTime < endTime
                then Success
                else Failure $ "must be before " <> show endTime
        _ -> Success

isAfter :: Maybe TimeOfDay -> Maybe TimeOfDay -> ValidatorResult
isAfter maybeStartTime maybeEndTime =
    case (maybeStartTime, maybeEndTime) of
        (Just startTime, Just endTime) ->
            if startTime < endTime
                then Success
                else Failure $ "must be after " <> show startTime
        _ -> Success

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

matchesClockDetails :: Maybe TimeOfDay -> Maybe TimeOfDay -> Maybe Int -> Double -> ValidatorResult
matchesClockDetails maybeClockedInAt maybeClockedOutAt maybeLunch hoursWorked =
    if clockDetailsMatchHoursWorked maybeClockedInAt maybeClockedOutAt maybeLunch hoursWorked
        then Success
        else Failure ("Must be within " <> show toleranceMinutes <> " minutes of clock details")

clockDetailsMatchHoursWorked :: Maybe TimeOfDay -> Maybe TimeOfDay -> Maybe Int -> Double -> Bool
clockDetailsMatchHoursWorked maybeClockedInAt maybeClockedOutAt maybeLunch hoursWorked =
    case clockDetailsToHoursWorked maybeClockedInAt maybeClockedOutAt maybeLunch of
        Just computedHoursWorked -> approximatelyEqual tolerance hoursWorked computedHoursWorked
        _ -> True
  where
    approximatelyEqual :: Double -> Double -> Double -> Bool
    approximatelyEqual tolerance a b = abs (a - b) <= tolerance
    tolerance :: Double
    tolerance = fromMinutes toleranceMinutes + epsilon
    epsilon = 0.001

clockDetailsToHoursWorked :: Maybe TimeOfDay -> Maybe TimeOfDay -> Maybe Int -> Maybe Double
clockDetailsToHoursWorked maybeClockedInAt maybeClockedOutAt maybeLunch =
    case (maybeClockedInAt, maybeClockedOutAt) of
        (Just clockedInAt, Just clockedOutAt) -> Just $ worked clockedInAt clockedOutAt lunch
        _ -> Nothing
  where
    worked :: TimeOfDay -> TimeOfDay -> Double -> Double
    worked start end lunch = (fromTimeOfDay end - fromTimeOfDay start) - lunch
    lunch :: Double
    lunch = fromMinutes $ toInteger $ fromMaybe 0 maybeLunch
    fromTimeOfDay :: TimeOfDay -> Double
    fromTimeOfDay timeOfDay = fromIntegral (timeOfDayToPicoseconds timeOfDay) / fromIntegral picosecondsInHour
    timeOfDayToPicoseconds :: TimeOfDay -> Integer
    timeOfDayToPicoseconds timeOfDay = diffTimeToPicoseconds (timeOfDayToTime timeOfDay)
    picosecondsInHour :: Integer
    picosecondsInHour = picosecondsInSecond * 60 * 60
    picosecondsInSecond = 1000000000000

setClockedInAt :: Text -> TimecardEntry -> TimecardEntry
setClockedInAt clockedInAt timecardEntry =
    if not $ isBlank clockedInAt
        then case Brain.Normalize.clockedInAt clockedInAt of
            Just clockedInAt ->
                timecardEntry |> set #clockedInAt (Just clockedInAt)
            Nothing ->
                timecardEntry |> attachFailure #clockedInAt timeFormatErrorMessage
        else timecardEntry |> set #clockedInAt Nothing

setClockedOutAt :: Text -> TimecardEntry -> TimecardEntry
setClockedOutAt clockedOutAt timecardEntry =
    if not $ isBlank clockedOutAt
        then case Brain.Normalize.clockedOutAt clockedOutAt of
            Just clockedOutAt ->
                timecardEntry |> set #clockedOutAt (Just clockedOutAt)
            Nothing ->
                timecardEntry |> attachFailure #clockedOutAt timeFormatErrorMessage
        else timecardEntry |> set #clockedOutAt Nothing

timeFormatErrorMessage :: Text
timeFormatErrorMessage = "should have the form hh:mm am/pm"

toleranceMinutes :: Integer
toleranceMinutes = 20

fromMinutes :: Integer -> Double
fromMinutes minutes = fromIntegral minutes / 60
