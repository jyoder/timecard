module Application.Brain.Decide where

import qualified Application.Brain.Orient as Orient
import Generated.Types
import IHP.Prelude

data Plan
    = CreateTimecardEntryAndScheduleReminder
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
    | SuspendReminder
        { actionRunStateId :: !(Id ActionRunState)
        }
    | DoNothing
    deriving (Eq, Show)

decide :: Orient.Situation -> Plan
decide Orient.Situation {..} =
    case update of
        Orient.UpdateIsForASingleJob Orient.Job {..} ->
            case reminder of
                Orient.ReminderIsNotScheduled ->
                    CreateTimecardEntryAndScheduleReminder
                        { jobName = name
                        , linkedMessageId = twilioMessageId
                        , ..
                        }
                Orient.ReminderIsScheduled {..} ->
                    SuspendReminder {..}
                Orient.ReminderIsSuspended ->
                    DoNothing
        Orient.UpdateIsForMultipleJobs -> DoNothing
        Orient.UpdateDetailsDoNotMatch -> DoNothing
        Orient.MessageIsNotAnUpdate -> DoNothing