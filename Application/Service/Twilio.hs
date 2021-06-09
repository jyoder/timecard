module Application.Service.Twilio (
    sendPhoneMessage,
    callbackSignature,
    accountId,
    authToken,
    statusCallbackUrl,
    AccountId (..),
    AuthToken (..),
    StatusCallbackUrl (..),
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

newtype AccountId = AccountId Text
newtype AuthToken = AuthToken Text
newtype StatusCallbackUrl = StatusCallbackUrl Text

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
    AccountId ->
    AuthToken ->
    StatusCallbackUrl ->
    Text ->
    Text ->
    Text ->
    IO Response
sendPhoneMessage
    (AccountId accountId)
    (AuthToken authToken)
    (StatusCallbackUrl statusCallbackUrl)
    fromPhoneNumber
    toPhoneNumber
    messageBody = do
        httpResponse <- runReq defaultHttpConfig $ do
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

accountId :: (?context :: context, ConfigProvider context) => AccountId
accountId =
    ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TMap.lookup @AccountId
        |> fromMaybe (error "Could not find Twilio.AccountId in config")

authToken :: (?context :: context, ConfigProvider context) => AuthToken
authToken =
    ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TMap.lookup @AuthToken
        |> fromMaybe (error "Could not find Twilio.AuthToken in config")

statusCallbackUrl :: (?context :: context, ConfigProvider context) => StatusCallbackUrl
statusCallbackUrl =
    ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TMap.lookup @StatusCallbackUrl
        |> fromMaybe (error "Could not find Twilio.StatusCallbackUrl in config")

post :: MonadHttp m => HttpBody body => Text -> Text -> Url 'Https -> body -> m BsResponse
post accountId authToken url body = req POST url body bsResponse auth
  where
    auth = Network.HTTP.Req.basicAuth (encodeUtf8 accountId) (encodeUtf8 authToken)

twilioEndpoint :: Text -> Url 'Https
twilioEndpoint accountId =
    https "api.twilio.com" /: "2010-04-01" /: "Accounts" /: accountId /: "Messages.json"