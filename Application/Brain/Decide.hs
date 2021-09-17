module Application.Brain.Decide where

import qualified Application.Brain.Orient as Orient
import Generated.Types
import IHP.Prelude

data Plan
    = CreateTimecardEntry
        { now :: !UTCTime
        , companyTimeZone :: !TimeZone
        , workerId :: !(Id Person)
        , botPhoneNumberId :: !(Id PhoneNumber)
        , workerPhoneNumberId :: !(Id PhoneNumber)
        , linkedMessageId :: !(Id TwilioMessage)
        , date :: !Day
        , jobName :: !Text
        , hoursWorked :: !Double
        , clockedInAt :: !(Maybe TimeOfDay)
        , clockedOutAt :: !(Maybe TimeOfDay)
        , lunchDuration :: !Int
        , workDone :: !Text
        , invoiceTranslation :: !Text
        }
    | SuspendScheduledMessages
        { actionRunStateId :: !(Id ActionRunState)
        }
    | DoNothing
    deriving (Eq, Show)

decide :: Orient.Situation -> Plan
decide Orient.Situation {..} =
    case reminder of
        Orient.ReminderIsNotScheduled ->
            case update of
                Orient.UpdateIsForASingleJob Orient.Job {..} ->
                    CreateTimecardEntry
                        { jobName = name
                        , linkedMessageId = twilioMessageId
                        , ..
                        }
                Orient.UpdateIsForMultipleJobs -> DoNothing
                Orient.UpdateDetailsAreLowConfidence -> DoNothing
                Orient.UpdateDetailsDoNotMatch -> DoNothing
                Orient.MessageIsNotAnUpdate -> DoNothing
        Orient.ReminderIsScheduled {..} ->
            SuspendScheduledMessages {..}
        Orient.ReminderIsSuspended ->
            DoNothing
