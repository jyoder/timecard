module Application.Brain.Observe where

import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Twilio.View as Twilio.View
import Generated.Types
import IHP.Prelude

data Event = IncomingMessage
    { message :: !Twilio.View.Message
    , workerId :: !(Id Person)
    , workerPhoneNumberId :: !(Id PhoneNumber)
    , botPhoneNumberId :: !(Id PhoneNumber)
    }

data Observations = Observations
    { now :: !UTCTime
    , companyTimeZone :: !TimeZone
    , today :: !Day
    , event :: !Event
    , timecardEntryRows :: ![Timecard.Query.Row]
    , scheduledReminders :: ![SendMessageAction.T]
    }

observe :: Event -> IO Observations
observe = undefined