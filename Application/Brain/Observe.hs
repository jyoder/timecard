module Application.Brain.Observe where

import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Twilio.View as Twilio.View
import Generated.Types
import IHP.Prelude
import Text.Read (read)

data Event = IncomingMessage
    { message :: !Twilio.View.Message
    , workerId :: !(Id Person)
    , workerPhoneNumberId :: !(Id PhoneNumber)
    , botPhoneNumberId :: !(Id PhoneNumber)
    }
    deriving (Eq, Show)

data Observations = Observations
    { now :: !UTCTime
    , companyTimeZone :: !TimeZone
    , today :: !Day
    , event :: !Event
    , timecardEntryRows :: ![Timecard.Query.Row]
    , scheduledReminders :: ![SendMessageAction.T]
    }
    deriving (Eq, Show)

observe :: (?modelContext :: ModelContext) => Event -> IO Observations
observe event = do
    now <- getCurrentTime
    let today = localDay $ utcToLocalTime companyTimeZone' now
    let companyTimeZone = companyTimeZone'
    let IncomingMessage {..} = event

    timecardEntryRows <-
        Timecard.Query.fetchByPerson
            Timecard.Query.EntriesDateDescending
            workerId

    scheduledReminders <-
        SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
            workerPhoneNumberId

    pure Observations {..}

companyTimeZone' :: TimeZone
companyTimeZone' = read "PST"