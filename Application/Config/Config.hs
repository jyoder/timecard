module Application.Config.Config (
    T (..),
    load,
) where

import Application.Config.Environment (findVar, loadEnvVars)
import qualified Application.Twilio.Client as Twilio.Client
import qualified Application.VertexAi.Client as VertexAi.Client
import IHP.Prelude

data T = T
    { twilioConfig :: Twilio.Client.Config
    , vertexAiConfig :: VertexAi.Client.Config
    }

load :: IO T
load = do
    vars <- loadEnvVars envVarNames
    case vars of
        Left message -> do
            putStrLn message
            error message
        Right vars ->
            pure $
                T
                    { twilioConfig = buildTwilioConfig vars
                    , vertexAiConfig = buildVertexAiConfig vars
                    }

buildTwilioConfig :: [(Text, Text)] -> Twilio.Client.Config
buildTwilioConfig vars =
    let enableIntegrations = findVar vars timecardEnableIntegrationsVarName
     in if enableIntegrations == "0"
            then Twilio.Client.DisabledConfig
            else
                Twilio.Client.EnabledConfig
                    { accountId = findVar vars twilioAccountIdVarName
                    , authToken = findVar vars twilioAuthTokenVarName
                    , statusCallbackUrl = findVar vars twilioStatusCallbackUrlVarName
                    }

buildVertexAiConfig :: [(Text, Text)] -> VertexAi.Client.Config
buildVertexAiConfig vars =
    let enableIntegrations = findVar vars timecardEnableIntegrationsVarName
     in if enableIntegrations == "0"
            then VertexAi.Client.DisabledConfig
            else
                VertexAi.Client.EnabledConfig
                    { endpointRegion = findVar vars vertexAiEndpointRegionVarName
                    , endpointProjectName = findVar vars vertexAiEndpointProjectNameVarName
                    , endpointId = findVar vars vertexAiEndpointIdVarName
                    , serviceAccountEmail = findVar vars vertexAiServiceAccountEmailVarName
                    , privateKeyId = findVar vars vertexAiPrivateKeyIdVarName
                    , privateKey = findVar vars vertexAiPrivateKeyVarName
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
    , vertexAiServiceAccountEmailVarName
    , vertexAiPrivateKeyIdVarName
    , vertexAiPrivateKeyVarName
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

vertexAiServiceAccountEmailVarName :: Text
vertexAiServiceAccountEmailVarName = "VERTEX_AI_SERVICE_ACCOUNT_EMAIL"

vertexAiPrivateKeyIdVarName :: Text
vertexAiPrivateKeyIdVarName = "VERTEX_AI_PRIVATE_KEY_ID"

vertexAiPrivateKeyVarName :: Text
vertexAiPrivateKeyVarName = "VERTEX_AI_PRIVATE_KEY"
