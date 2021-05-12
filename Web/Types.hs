module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data PersonsController
    = PersonsAction
    | NewPersonAction
    | ShowPersonAction { personId :: !(Id Person) }
    | CreatePersonAction
    | EditPersonAction { personId :: !(Id Person) }
    | UpdatePersonAction { personId :: !(Id Person) }
    | DeletePersonAction { personId :: !(Id Person) }
    deriving (Eq, Show, Data)

data PhoneNumbersController
    = PhoneNumbersAction
    | NewPhoneNumberAction
    | ShowPhoneNumberAction { phoneNumberId :: !(Id PhoneNumber) }
    | CreatePhoneNumberAction
    | EditPhoneNumberAction { phoneNumberId :: !(Id PhoneNumber) }
    | UpdatePhoneNumberAction { phoneNumberId :: !(Id PhoneNumber) }
    | DeletePhoneNumberAction { phoneNumberId :: !(Id PhoneNumber) }
    deriving (Eq, Show, Data)
