module Config where

import qualified Application.Service.Twilio as Twilio
import Data.Text (pack)
import IHP.Environment
import IHP.FrameworkConfig
import qualified IHP.Log as Log
import IHP.Log.Types
import IHP.Prelude
import System.Environment (getEnv)

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    logger <-
        liftIO $
            newLogger
                def
                    { level = Debug
                    , formatter = withTimeFormatter
                    }
    option logger

    twilioAccountId <- liftIO $ getEnv "TWILIO_ACCOUNT_ID"
    option (Twilio.AccountId $ pack twilioAccountId)

    twilioAuthToken <- liftIO $ getEnv "TWILIO_AUTH_TOKEN"
    option (Twilio.AuthToken $ pack twilioAuthToken)

    twilioStatusCallbackUrl <- liftIO $ getEnv "TWILIO_STATUS_CALLBACK_URL"
    option (Twilio.StatusCallbackUrl $ pack twilioStatusCallbackUrl)
