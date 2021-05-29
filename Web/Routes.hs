module Web.Routes where

import Generated.Types
import IHP.RouterPrelude
import Web.Types

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute SessionsController
instance AutoRoute PeopleController
instance AutoRoute PhoneNumbersController
instance AutoRoute PhoneContactsController
instance AutoRoute CommunicationsController
instance AutoRoute TwilioMessagesController

instance AutoRoute TwilioCallbacksController

