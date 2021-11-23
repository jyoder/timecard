module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.AuditEntries
import Web.Controller.Reports
import Web.Controller.TimecardReviews
import Web.Controller.Timecards
import Web.Controller.TwilioCallbacks
import Web.Controller.WorkerSettings

import IHP.LoginSupport.Middleware
import Web.Controller.Communications
import Web.Controller.People
import Web.Controller.PhoneContacts
import Web.Controller.PhoneNumbers
import Web.Controller.Sessions
import Web.Controller.Static

instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        , parseRoute @SessionsController
        , -- Generator Marker
          parseRoute @ReportsController
        , parseRoute @AuditEntriesController
        , parseRoute @WorkerSettingsController
        , parseRoute @TimecardReviewsController
        , parseRoute @TimecardsController
        , parseRoute @TwilioCallbacksController
        , parseRoute @CommunicationsController
        , parseRoute @PhoneContactsController
        , parseRoute @PhoneNumbersController
        , parseRoute @PeopleController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @User
