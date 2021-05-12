module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.PhoneContacts
import Web.Controller.PhoneNumbers
import Web.Controller.Persons
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @PhoneContactsController
        , parseRoute @PhoneNumbersController
        , parseRoute @PersonsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
