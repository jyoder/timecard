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
        { timecardEntryId :: !(Id TimecardEntry)
        }
    | CreateOutgoingPhoneMessageAction
    | CancelScheduledMessageAction
        { sendMessageActionId :: !(Id SendMessageAction)
        }
    | CreateTimecardReview
    deriving (Eq, Show, Data)

data TwilioCallbacksController
    = UpdateOutgoingPhoneMessageAction
    | CreateIncomingPhoneMessageAction
    deriving (Eq, Show, Data)

data TimecardsController
    = TimecardsAction
    | TimecardPersonSelectionAction
        { selectedPersonId :: !(Id Person)
        }
    | TimecardEditTimecardEntryAction
        { timecardEntryId :: !(Id TimecardEntry)
        }
    | TimecardDownloadTimecardAction
        { timecardId :: !(Id Timecard)
        }
    | TimecardUpdateTimecardEntryAction
        { timecardEntryId :: !(Id TimecardEntry)
        }
    deriving (Eq, Show, Data)

data TimecardReviewsController
    = ShowTimecardReviewAction
        { accessToken :: !Text
        }
    | CreateSigningAction
    deriving (Eq, Show, Data)

data WorkerPreferencesController
    = WorkerPreferencesAction
    | NewWorkerPreferenceAction
    | ShowWorkerPreferenceAction {workerPreferenceId :: !(Id WorkerPreference)}
    | CreateWorkerPreferenceAction
    | EditWorkerPreferenceAction {workerPreferenceId :: !(Id WorkerPreference)}
    | UpdateWorkerPreferenceAction {workerPreferenceId :: !(Id WorkerPreference)}
    | DeleteWorkerPreferenceAction {workerPreferenceId :: !(Id WorkerPreference)}
    deriving (Eq, Show, Data)
