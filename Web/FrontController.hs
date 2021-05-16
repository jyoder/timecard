module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.TwilioMessages

import Web.Controller.Communications
import Web.Controller.Persons
import Web.Controller.PhoneContacts
import Web.Controller.PhoneMessages
import Web.Controller.PhoneNumbers
import Web.Controller.Static

instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        , -- Generator Marker
          parseRoute @TwilioMessagesController
        , parseRoute @CommunicationsController
        , parseRoute @PhoneMessagesController
        , parseRoute @PhoneContactsController
        , parseRoute @PhoneNumbersController
        , parseRoute @PersonsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
