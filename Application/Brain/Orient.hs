module Application.Brain.Orient where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Brain.Normalize as Normalize
import qualified Application.Brain.Observe as Observe
import Application.Service.Time (nextWorkingDay)
import qualified Application.Timecard.EntryRequest as EntryRequest
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Twilio.Query as Twilio.Query
import qualified Application.Twilio.View as Twilio.View
import Generated.Types
import IHP.Prelude

data Situation = Situation
    { now :: !UTCTime
    , companyTimeZone :: !TimeZone
    , workerId :: !(Id Person)
    , botPhoneNumberId :: !(Id PhoneNumber)
    , workerPhoneNumberId :: !(Id PhoneNumber)
    , twilioMessageId :: !(Id TwilioMessage)
    , update :: !Update
    , reminder :: !Reminder
    }
    deriving (Eq, Show)

data Update
    = UpdateIsForASingleJob Job
    | UpdateIsForMultipleJobs
    | UpdateDetailsDoNotMatch
    | MessageIsNotAnUpdate
    deriving (Eq, Show)

data Reminder
    = ReminderIsNotScheduled
    | ReminderIsScheduled
        { actionRunStateId :: !(Id ActionRunState)
        }
    | ReminderIsSuspended
    deriving (Eq, Show)

data Job = Job
    { date :: !Day
    , name :: !Text
    , clockedInAt :: !(Maybe TimeOfDay)
    , clockedOutAt :: !(Maybe TimeOfDay)
    , lunchDuration :: !Int
    , hoursWorked :: !Double
    , workDone :: !Text
    , invoiceTranslation :: !Text
    }
    deriving (Eq, Show)

data Message = Message
    { jobName :: ![Text]
    , clockedInAt :: ![TimeOfDay]
    , clockedOutAt :: ![TimeOfDay]
    , hoursWorked :: ![Double]
    , workDone :: !Text
    , invoiceTranslation :: !Text
    }
    deriving (Eq, Show)

orient :: Observe.Observations -> Situation
orient Observe.Observations {..} =
    case event of
        Observe.IncomingMessage {..} ->
            Situation
                { twilioMessageId = get #id message
                , update = buildUpdate today timecardEntryRows message
                , reminder = buildReminder scheduledReminders
                , ..
                }

buildUpdate :: Day -> [Timecard.Query.Row] -> Twilio.View.Message -> Update
buildUpdate today timecardEntryRows message =
    case jobs of
        [job] ->
            if jobDetailsMatch job
                then UpdateIsForASingleJob job
                else UpdateDetailsDoNotMatch
        _ : _ -> UpdateIsForMultipleJobs
        [] -> MessageIsNotAnUpdate
  where
    jobs = message |> normalizedMessage previousJobName |> buildJobs date
    previousJobName = get #timecardEntryJobName <$> head timecardEntryRows
    date = nextTimecardEntryDay today timecardEntryDays
    timecardEntryDays = get #timecardEntryDate <$> timecardEntryRows

buildReminder :: [SendMessageAction.T] -> Reminder
buildReminder (SendMessageAction.T {..} : _)
    | state == ActionRunState.notStarted = ReminderIsScheduled {..}
    | state == ActionRunState.suspended = ReminderIsSuspended
    | otherwise = ReminderIsNotScheduled
buildReminder [] = ReminderIsNotScheduled

buildJobs :: Day -> Message -> [Job]
buildJobs date Message {..} =
    map (\(name, clockedInAt, clockedOutAt, hoursWorked) -> Job {..}) jobTuples
  where
    jobTuples = tuples |> inferHours |> dropMissingHours |> guaranteeHours
    tuples = zip4 jobName clockedInAt' clockedOutAt' hoursWorked'
    inferHours = map (\(jn, cIn, cOut, hrs) -> (jn, cIn, cOut, inferHoursWorked cIn cOut hrs))
    dropMissingHours = filter (\(_, _, _, hoursWorked) -> isJust hoursWorked)
    guaranteeHours = map (\(n, cIn, cOut, hrs) -> (n, cIn, cOut, fromMaybe 8.0 hrs))
    clockedInAt' = (Just <$> clockedInAt) <> repeat (Nothing :: Maybe TimeOfDay)
    clockedOutAt' = (Just <$> clockedOutAt) <> repeat (Nothing :: Maybe TimeOfDay)
    hoursWorked' = (Just <$> hoursWorked) <> repeat (Nothing :: Maybe Double)
    lunchDuration = assumedLunchDuration

inferHoursWorked :: Maybe TimeOfDay -> Maybe TimeOfDay -> Maybe Double -> Maybe Double
inferHoursWorked clockedInAt clockedOutAt hoursWorked =
    case (clockedInAt, clockedOutAt, hoursWorked) of
        (_, _, Just hoursWorked) -> Just hoursWorked
        (Just clockedInAt, Just clockedOutAt, Nothing) ->
            let minutesWorked = minutesBetween clockedInAt clockedOutAt - assumedLunchDuration
             in Just $ fromIntegral minutesWorked / 60.0
        _ -> Nothing

nextTimecardEntryDay :: Day -> [Day] -> Day
nextTimecardEntryDay today timecardEntryDays =
    maybe today nextWorkingDay lastTimecardEntryDay
  where
    lastTimecardEntryDay = head timecardEntryDays

normalizedMessage :: Maybe Text -> Twilio.View.Message -> Message
normalizedMessage previousJobName Twilio.View.Message {..} =
    Message
        { jobName = normalizedJobNames previousJobName highConfidenceEntities'
        , clockedInAt = normalizedClockedInAts highConfidenceEntities'
        , clockedOutAt = normalizedClockedOutAts highConfidenceEntities'
        , hoursWorked = normalizedHoursWorked highConfidenceEntities'
        , workDone = body
        , invoiceTranslation = body
        }
  where
    highConfidenceEntities' = highConfidenceEntities entities

normalizedJobNames :: Maybe Text -> [Twilio.View.Entity] -> [Text]
normalizedJobNames previousJobName entities =
    catMaybes
        if null normalizedJobNames
            then [previousJobName]
            else normalizedJobNames
  where
    normalizedJobNames = Normalize.jobName previousJobName <$> jobNames
    jobNames = Just . get #rawText <$> jobNameEntities
    jobNameEntities = findEntities Twilio.Query.JobName entities

normalizedClockedInAts :: [Twilio.View.Entity] -> [TimeOfDay]
normalizedClockedInAts entities = catMaybes $ Normalize.clockedInAt <$> clockedInAts
  where
    clockedInAts = get #rawText <$> clockedInAtEntities
    clockedInAtEntities = findEntities Twilio.Query.ClockedInAt entities

normalizedClockedOutAts :: [Twilio.View.Entity] -> [TimeOfDay]
normalizedClockedOutAts entities = catMaybes $ Normalize.clockedOutAt <$> clockedOutAts
  where
    clockedOutAts = get #rawText <$> clockedOutAtEntities
    clockedOutAtEntities = findEntities Twilio.Query.ClockedOutAt entities

normalizedHoursWorked :: [Twilio.View.Entity] -> [Double]
normalizedHoursWorked entities = catMaybes $ Normalize.hoursWorked <$> hoursWorkeds
  where
    hoursWorkeds = get #rawText <$> hoursWorkedEntities
    hoursWorkedEntities = findEntities Twilio.Query.HoursWorked entities

findEntities :: Twilio.Query.EntityType -> [Twilio.View.Entity] -> [Twilio.View.Entity]
findEntities entityType = filter (\entity -> get #entityType entity == entityType)

highConfidenceEntities :: [Twilio.View.Entity] -> [Twilio.View.Entity]
highConfidenceEntities = filter (\entity -> get #confidence entity >= entityConfidenceThreshold)

jobDetailsMatch :: Job -> Bool
jobDetailsMatch Job {..} =
    case (clockedInAt, clockedOutAt) of
        (Just clockedInAt, Just clockedOutAt) ->
            let minutesWorkedByClock = minutesBetween clockedInAt clockedOutAt - lunchDuration
                minutesWorked = hoursToMinutes hoursWorked
             in abs (minutesWorked - minutesWorkedByClock) <= minutesWorkedTolerance
        _ -> True -- For now, assume details match if clock info was not provided

minutesBetween :: TimeOfDay -> TimeOfDay -> Int
minutesBetween start end = round durationMinutes
  where
    durationMinutes = picosecondsToSeconds durationPicoseconds / 60.0
    durationPicoseconds = diffTimeToPicoseconds duration
    duration = timeOfDayToTime end - timeOfDayToTime start
    picosecondsToSeconds picoseconds = fromInteger picoseconds / 1000000000000

hoursToMinutes :: Double -> Int
hoursToMinutes hours = round $ hours * 60

entityConfidenceThreshold :: Double
entityConfidenceThreshold = 0.8

assumedLunchDuration :: Int
assumedLunchDuration = 30

minutesWorkedTolerance :: Int
minutesWorkedTolerance = 15
