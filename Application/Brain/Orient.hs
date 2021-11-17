module Application.Brain.Orient where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Brain.Normalize as Normalize
import qualified Application.Brain.Observe as Observe
import Application.Service.Time (nextWorkingDay)
import qualified Application.Timecard.Entry as Timecard.Entry
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
    | UpdateDetailsAreLowConfidence
    | UpdateDetailsDoNotMatch
    | MessageIsNotAnUpdate
    deriving (Eq, Show)

data Reminder
    = ReminderIsNotScheduled
    | ReminderIsScheduled
        { actionRunStateIds :: ![Id ActionRunState]
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
buildUpdate today timecardEntryRows message
    | anyLowConfidenceEntities entities = UpdateDetailsAreLowConfidence
    | hasMultipleJobNames entities = UpdateIsForMultipleJobs
    | otherwise =
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
    entities = get #entities message

buildReminder :: [SendMessageAction.T] -> Reminder
buildReminder sendMessageActions
    | null sendMessageActions = ReminderIsNotScheduled
    | allActionsSuspended = ReminderIsSuspended
    | anyActionsNotStarted = ReminderIsScheduled {..}
    | otherwise = ReminderIsNotScheduled
  where
    allActionsSuspended = all isActionSuspended sendMessageActions
    anyActionsNotStarted = any isActionNotStarted sendMessageActions
    isActionSuspended action = get #state action == ActionRunState.suspended
    isActionNotStarted action = get #state action == ActionRunState.notStarted
    actionRunStateIds = get #actionRunStateId <$> notStartedActions
    notStartedActions = filter isActionNotStarted sendMessageActions

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
        { jobName = normalizedJobNames previousJobName entities
        , clockedInAt = normalizedClockedInAts entities
        , clockedOutAt = normalizedClockedOutAts entities
        , hoursWorked = normalizedHoursWorked entities
        , workDone = body
        , invoiceTranslation = body
        }

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

anyLowConfidenceEntities :: [Twilio.View.Entity] -> Bool
anyLowConfidenceEntities = any (\entity -> get #confidence entity < entityConfidenceThreshold)

hasMultipleJobNames :: [Twilio.View.Entity] -> Bool
hasMultipleJobNames entities = length (findEntities Twilio.Query.JobName entities) > 1

findEntities :: Twilio.Query.EntityType -> [Twilio.View.Entity] -> [Twilio.View.Entity]
findEntities entityType = filter (\entity -> get #entityType entity == entityType)

jobDetailsMatch :: Job -> Bool
jobDetailsMatch Job {..} =
    Timecard.Entry.clockDetailsMatchHoursWorked
        clockedInAt
        clockedOutAt
        (Just lunchDuration)
        hoursWorked

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
