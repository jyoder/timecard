module Web.Types where

import Generated.Types
import IHP.LoginSupport.Types
import IHP.ModelSupport
import IHP.Prelude

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)

instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)

data PeopleController
    = PeopleAction
    | NewPersonAction
    | ShowPersonAction {personId :: !(Id Person)}
    | CreatePersonAction
    | EditPersonAction {personId :: !(Id Person)}
    | UpdatePersonAction {personId :: !(Id Person)}
    | DeletePersonAction {personId :: !(Id Person)}
    deriving (Eq, Show, Data)

data PhoneNumbersController
    = PhoneNumbersAction
    | NewPhoneNumberAction
    | ShowPhoneNumberAction {phoneNumberId :: !(Id PhoneNumber)}
    | CreatePhoneNumberAction
    | EditPhoneNumberAction {phoneNumberId :: !(Id PhoneNumber)}
    | UpdatePhoneNumberAction {phoneNumberId :: !(Id PhoneNumber)}
    | DeletePhoneNumberAction {phoneNumberId :: !(Id PhoneNumber)}
    deriving (Eq, Show, Data)

data PhoneContactsController
    = PhoneContactsAction
    | NewPhoneContactAction
    | ShowPhoneContactAction {phoneContactId :: !(Id PhoneContact)}
    | CreatePhoneContactAction
    | EditPhoneContactAction {phoneContactId :: !(Id PhoneContact)}
    | UpdatePhoneContactAction {phoneContactId :: !(Id PhoneContact)}
    | DeletePhoneContactAction {phoneContactId :: !(Id PhoneContact)}
    deriving (Eq, Show, Data)

data CommunicationsController
    = CommunicationsAction
    | PersonSelectionAction
        { selectedPersonId :: !(Id Person)
        }
    | NewTimecardEntryAction
        { selectedPersonId :: !(Id Person)
        , selectedMessageIds :: ![Text]
        }
    | EditTimecardEntryAction
        { selectedPersonId :: !(Id Person)
        , timecardEntryId :: !(Id TimecardEntry)
        }
    | EditModifiedTimecardEntryAction
        { selectedPersonId :: !(Id Person)
        , timecardEntryId :: !(Id TimecardEntry)
        , selectedMessageIds :: ![Text]
        }
    | CreateTimecardEntryAction
    | UpdateTimecardEntryAction
    | CreateOutgoingPhoneMessageAction
    deriving (Eq, Show, Data)

data TwilioMessagesController
    = TwilioMessagesAction
    | NewTwilioMessageAction
    | ShowTwilioMessageAction {twilioMessageId :: !(Id TwilioMessage)}
    | CreateTwilioMessageAction
    | EditTwilioMessageAction {twilioMessageId :: !(Id TwilioMessage)}
    | UpdateTwilioMessageAction {twilioMessageId :: !(Id TwilioMessage)}
    | DeleteTwilioMessageAction {twilioMessageId :: !(Id TwilioMessage)}
    deriving (Eq, Show, Data)

data TwilioCallbacksController
    = UpdateOutgoingPhoneMessageAction
    | CreateIncomingPhoneMessageAction
    deriving (Eq, Show, Data)
