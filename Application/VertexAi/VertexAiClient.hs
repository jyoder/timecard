module Application.VertexAi.VertexAiClient (
    Config (..),
    Response,
    predict,
) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.TMap as TMap
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import IHP.ControllerPrelude
import Network.HTTP.Req

data Config
    = EnabledConfig
        { endpointRegion :: !Text
        , endpointProjectName :: !Text
        , endpointId :: !Text
        , authToken :: !Text
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
    { ids :: ![Text]
    , displayNames :: ![Text]
    , textSegmentStartOffsets :: ![Int]
    , textSegmentEndOffsets :: ![Int]
    , confidences :: ![Double]
    }
    deriving (Show, Generic)

instance ToJSON Response
instance FromJSON Response

predict :: Config -> Text -> IO Response
predict EnabledConfig {..} content = do
    response <-
        runReq
            defaultHttpConfig
            (post endpoint' authToken body)
    pure (responseBody response :: Response)
  where
    endpoint' = endpoint endpointRegion endpointProjectName endpointId
    body = ReqBodyJson request
    request = Request {instances = [Instance {mimeType, content}]}
    mimeType = "text/plain"
predict DisabledConfig _ = undefined

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

config :: (?context :: context, ConfigProvider context) => Config
config =
    ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TMap.lookup @Config
        |> \case
            Just config -> config
            Nothing -> error "Missing configuration VertexAiClient.Config"
