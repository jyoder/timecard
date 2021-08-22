module Application.Config.Config (
    load,
) where

import Application.Config.Environment (findVar, loadEnvVars)
import qualified Application.Twilio.TwilioClient as TwilioClient
import qualified Application.VertexAi.VertexAiClient as VertexAiClient
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

buildVertexAiConfig :: [(Text, Text)] -> VertexAiClient.Config
buildVertexAiConfig vars =
    let enableIntegrations = findVar vars timecardEnableIntegrationsVarName
     in if enableIntegrations == "0"
            then VertexAiClient.DisabledConfig
            else
                VertexAiClient.EnabledConfig
                    { endpointRegion = findVar vars vertexAiEndpointRegionVarName
                    , endpointProjectName = findVar vars vertexAiEndpointProjectNameVarName
                    , endpointId = findVar vars vertexAiEndpointIdVarName
                    , authToken = findVar vars vertexAiAuthTokenVarName
                    }

envVarNames :: [Text]
envVarNames =
    [ timecardEnableIntegrationsVarName
    , twilioAccountIdVarName
    , twilioAuthTokenVarName
    , twilioStatusCallbackUrlVarName
    , vertexAiEndpointRegionVarName
    , vertexAiEndpointProjectNameVarName
    , vertexAiEndpointIdVarName
    , vertexAiAuthTokenVarName
    ]

timecardEnableIntegrationsVarName :: Text
timecardEnableIntegrationsVarName = "TIMECARD_ENABLE_INTEGRATIONS"

twilioAccountIdVarName :: Text
twilioAccountIdVarName = "TWILIO_ACCOUNT_ID"

twilioAuthTokenVarName :: Text
twilioAuthTokenVarName = "TWILIO_AUTH_TOKEN"

twilioStatusCallbackUrlVarName :: Text
twilioStatusCallbackUrlVarName = "TWILIO_STATUS_CALLBACK_URL"

vertexAiEndpointRegionVarName :: Text
vertexAiEndpointRegionVarName = "VERTEX_AI_ENDPOINT_REGION"

vertexAiEndpointProjectNameVarName :: Text
vertexAiEndpointProjectNameVarName = "VERTEX_AI_ENDPOINT_PROJECT_NAME"

vertexAiEndpointIdVarName :: Text
vertexAiEndpointIdVarName = "VERTEX_AI_ENDPOINT_ID"

vertexAiAuthTokenVarName :: Text
vertexAiAuthTokenVarName = "VERTEX_AI_AUTH_TOKEN"
