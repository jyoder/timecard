module Application.Config.Config (
    load,
) where

import Application.Config.Environment (findVar, loadEnvVars)
import qualified Application.Twilio.TwilioClient as TwilioClient
import IHP.Prelude

load :: IO TwilioClient.Config
load = do
    vars <- loadEnvVars envVarNames
    case vars of
        Left message -> do
            putStrLn message
            error message
        Right vars ->
            pure $ buildTwilioConfig vars

buildTwilioConfig :: [(Text, Text)] -> TwilioClient.Config
buildTwilioConfig vars =
    let enableIntegrations = findVar vars timecardEnableIntegrationsVarName
     in if enableIntegrations == "0"
            then TwilioClient.DisabledConfig
            else
                TwilioClient.EnabledConfig
                    { accountId = findVar vars twilioAccountIdVarName
                    , authToken = findVar vars twilioAuthTokenVarName
                    , statusCallbackUrl = findVar vars twilioStatusCallbackUrlVarName
                    }

envVarNames :: [Text]
envVarNames =
    [ timecardEnableIntegrationsVarName
    , twilioAccountIdVarName
    , twilioAuthTokenVarName
    , twilioStatusCallbackUrlVarName
    ]

timecardEnableIntegrationsVarName :: Text
timecardEnableIntegrationsVarName = "TIMECARD_ENABLE_INTEGRATIONS"

twilioAccountIdVarName :: Text
twilioAccountIdVarName = "TWILIO_ACCOUNT_ID"

twilioAuthTokenVarName :: Text
twilioAuthTokenVarName = "TWILIO_AUTH_TOKEN"

twilioStatusCallbackUrlVarName :: Text
twilioStatusCallbackUrlVarName = "TWILIO_STATUS_CALLBACK_URL"
