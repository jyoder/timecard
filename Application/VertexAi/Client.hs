module Application.VertexAi.Client (
    Config (..),
    Response (..),
    Prediction (..),
    predict,
    config,
) where

import qualified Application.VertexAi.Jwt as Jwt
import qualified Data.TMap as TMap
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import IHP.ControllerPrelude
import Network.HTTP.Req

data Config
    = EnabledConfig
        { endpointRegion :: !Text
        , endpointProjectName :: !Text
        , endpointId :: !Text
        , serviceAccountEmail :: !Text
        , privateKeyId :: !Text
        , privateKey :: !Text
        }
    | DisabledConfig

newtype Request = Request
    { instances :: [Instance]
    }
    deriving (Show, Generic)

instance ToJSON Request
instance FromJSON Request

data Instance = Instance
    { mimeType :: !Text
    , content :: !Text
    }
    deriving (Show, Generic)

instance ToJSON Instance
instance FromJSON Instance

data Response = Response
    { predictions :: ![Prediction]
    , deployedModelId :: !Text
    }
    deriving (Show, Generic)

instance ToJSON Response
instance FromJSON Response

data Prediction = Prediction
    { ids :: ![Text]
    , displayNames :: ![Text]
    , textSegmentStartOffsets :: ![Int]
    , textSegmentEndOffsets :: ![Int]
    , confidences :: ![Double]
    }
    deriving (Show, Generic)

instance ToJSON Prediction
instance FromJSON Prediction

newtype ClientException = ClientException
    { description :: Text
    }
    deriving (Show, Eq)

instance Exception ClientException where
    displayException ClientException {description} =
        Text.unpack $ "Failed to obtain prediction: " <> description

config :: (?context :: context, ConfigProvider context) => Config
config =
    ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TMap.lookup @Config
        |> \case
            Just config -> config
            Nothing -> error "Missing configuration VertexAi.Client.Config"

predict :: Config -> UTCTime -> Text -> IO Response
predict EnabledConfig {..} now content = do
    case jwt of
        Right jwt -> sendRequest endpoint' jwt content
        Left errorMessage -> throwIO $ ClientException errorMessage
  where
    jwt = Jwt.make Jwt.Params {..}
    serviceName = "https://" <> endpointRegion <> "-aiplatform.googleapis.com/"
    endpoint' = endpoint endpointRegion endpointProjectName endpointId
predict DisabledConfig _ _ = pure emptyResponse

sendRequest :: Url 'Https -> Text -> Text -> IO Response
sendRequest endpoint jwt content = do
    response <-
        runReq
            defaultHttpConfig
            (post endpoint jwt body)
    pure (responseBody response :: Response)
  where
    body = ReqBodyJson request
    request = Request {instances = [Instance {mimeType, content}]}
    mimeType = "text/plain"

post :: MonadHttp m => HttpBody body => Url 'Https -> Text -> body -> m (JsonResponse Response)
post url authToken body = req POST url body jsonResponse auth
  where
    auth =
        oAuth2Bearer (encodeUtf8 authToken)
            <> header "Content-Type" "application/vnd.api+json"

endpoint :: Text -> Text -> Text -> Url 'Https
endpoint region projectName endpointId =
    https domain /: "v1" /: "projects" /: projectName /: "locations" /: region /: "endpoints" /: endpointAction
  where
    domain = region <> "-aiplatform.googleapis.com"
    endpointAction = endpointId <> ":predict"

emptyResponse :: Response
emptyResponse =
    Response
        { predictions = []
        , deployedModelId = "dummy-model-id"
        }