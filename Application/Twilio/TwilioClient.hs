module Application.Twilio.TwilioClient (
    sendPhoneMessage,
    callbackSignature,
    config,
    Config (..),
    Response (..),
) where

import Control.Lens ((^?))
import Crypto.Hash.Algorithms (SHA1)
import "cryptonite" Crypto.MAC.HMAC (HMAC (hmacGetDigest), hmac)
import Data.Aeson.Lens (key, values, _Integer, _String)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.TMap as TMap
import Data.Text.Encoding (encodeUtf8)
import IHP.ControllerPrelude
import IHP.Prelude
import Network.HTTP.Req
import System.Random (randomIO)

data Config
    = EnabledConfig
        { accountId :: !Text
        , authToken :: !Text
        , statusCallbackUrl :: !Text
        }
    | DisabledConfig

data Response = Response
    { apiVersion :: Text
    , messageSid :: Text
    , accountSid :: Text
    , status :: Text
    , body :: Text
    , numMedia :: Int
    }
    deriving (Show)

sendPhoneMessage ::
    Config ->
    Text ->
    Text ->
    Text ->
    IO Response
sendPhoneMessage
    EnabledConfig {..}
    fromPhoneNumber
    toPhoneNumber
    messageBody = do
        httpResponse <- runReq defaultHttpConfig do
            post
                accountId
                authToken
                (twilioEndpoint accountId)
                (ReqBodyUrlEnc payload)
        case parseResponse httpResponse of
            Just response -> pure response
            Nothing -> error $ "Failed to parse Twilio response: " <> show httpResponse
      where
        payload =
            "From" =: fromPhoneNumber
                <> "To" =: toPhoneNumber
                <> "Body" =: messageBody
                <> "StatusCallback" =: statusCallbackUrl
sendPhoneMessage
    DisabledConfig
    fromPhoneNumber
    toPhoneNumber
    messageBody = do
        messageSid <- randomIO :: IO Int
        pure
            Response
                { apiVersion = "fakeApiVersion"
                , messageSid = "fakeSid-" <> show (abs messageSid)
                , accountSid = "fakeAccountSid"
                , status = "delivered"
                , body = messageBody
                , numMedia = 0
                }

parseResponse :: BsResponse -> Maybe Response
parseResponse httpResponse =
    Response
        <$> apiVersion body
        <*> messageSid body
        <*> accountSid body
        <*> messageStatus body
        <*> mBody body
        <*> numMedia body
  where
    body = responseBody httpResponse
    apiVersion body = body ^? key "api_version" . _String
    messageSid body = body ^? key "sid" . _String
    accountSid body = body ^? key "account_sid" . _String
    messageStatus body = body ^? key "status" . _String
    mBody body = body ^? key "body" . _String
    numMedia body = Just 0 -- For now we don't handle media

callbackSignature :: ByteString -> ByteString -> [(ByteString, Maybe ByteString)] -> ByteString
callbackSignature authToken url postParams =
    (hmac authToken urlWithParams :: HMAC SHA1) |> hmacGetDigest |> convert |> B64.encode
  where
    urlWithParams = url <> BS.intercalate "" concatedParams
    concatedParams = map (\(param, value) -> param <> fromMaybe "" value) sortedParams
    sortedParams = sortBy (\(a, _) (b, _) -> a `compare` b) postParams

post :: MonadHttp m => HttpBody body => Text -> Text -> Url 'Https -> body -> m BsResponse
post accountId authToken url body = req POST url body bsResponse auth
  where
    auth = Network.HTTP.Req.basicAuth (encodeUtf8 accountId) (encodeUtf8 authToken)

twilioEndpoint :: Text -> Url 'Https
twilioEndpoint accountId =
    https "api.twilio.com" /: "2010-04-01" /: "Accounts" /: accountId /: "Messages.json"

config :: (?context :: context, ConfigProvider context) => Config
config =
    ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TMap.lookup @Config
        |> \case
            Just config -> config
            Nothing -> error "Missing configuration TwilioClient.Config"
