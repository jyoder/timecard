module Application.Brain.Observe where

import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Timecard.View as Timecard.View
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
    , recentTimecards :: ![Timecard.View.Timecard]
    , scheduledReminders :: ![SendMessageAction.T]
    }
    deriving (Eq, Show)

observe :: (?modelContext :: ModelContext) => Event -> IO Observations
observe event = do
    now <- getCurrentTime
    let today = localDay $ utcToLocalTime companyTimeZone' now
    let companyTimeZone = companyTimeZone'
    let IncomingMessage {..} = event

    recentTimecards <-
        Timecard.View.buildTimecards
            <$> Timecard.Query.fetchByPerson
                Timecard.Query.EntriesDateDescending
                workerId

    scheduledReminders <-
        SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
            workerPhoneNumberId

    pure Observations {..}

companyTimeZone' :: TimeZone
companyTimeZone' = read "PST"