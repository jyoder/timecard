module Application.Service.Twilio (
    sendPhoneMessage,
    callbackSignature,
    AccountId (..),
    AuthToken (..),
    StatusCallbackUrl (..),
) where

import Crypto.Hash.Algorithms (SHA1)
import "cryptonite" Crypto.MAC.HMAC (HMAC (hmacGetDigest), hmac)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding (encodeUtf8)
import IHP.Prelude
import Network.HTTP.Req

newtype AccountId = AccountId Text
newtype AuthToken = AuthToken Text
newtype StatusCallbackUrl = StatusCallbackUrl Text

sendPhoneMessage :: AccountId -> AuthToken -> StatusCallbackUrl -> Text -> Text -> Text -> IO ()
sendPhoneMessage
    (AccountId accountId)
    (AuthToken authToken)
    (StatusCallbackUrl statusCallbackUrl)
    fromPhoneNumber
    toPhoneNumber
    messageBody = do
        runReq defaultHttpConfig $ do
            post
                accountId
                authToken
                (twilioEndpoint accountId)
                (ReqBodyUrlEnc payload)
            pure ()
      where
        payload =
            "From" =: fromPhoneNumber
                <> "To" =: toPhoneNumber
                <> "Body" =: messageBody
                <> "StatusCallback" =: statusCallbackUrl

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