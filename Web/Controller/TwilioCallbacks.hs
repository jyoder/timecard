module Web.Controller.TwilioCallbacks where

import qualified Application.Service.Twilio as Twilio
import qualified Data.TMap as TMap
import Data.Text.Encoding (encodeUtf8)
import qualified IHP.Log as Log
import Network.HTTP.Types (hContentType, status400)
import Network.Wai (responseLBS)
import Web.Controller.Prelude

instance Controller TwilioCallbacksController where
    action UpdateOutgoingPhoneMessageAction = do
        validateCallbackSignature
        let messageSid = param @Text "MessageSid"
        let messageStatus = param @Text "MessageStatus"

        twilioMessage <-
            query @TwilioMessage
                |> filterWhere (#messageSid, messageSid)
                |> fetchOne

        if get #status twilioMessage /= "delivered"
            then do
                twilioMessage
                    |> set #status messageStatus
                    |> updateRecord
                pure ()
            else pure ()

        renderPlain ""
    --
    action CreateIncomingPhoneMessageAction = do
        validateCallbackSignature
        let twilioMessage = buildIncomingTwilioMessage newRecord
        let fromNumber = param "From"
        let toNumber = param "To"

        fromPhoneNumber <-
            query @PhoneNumber
                |> filterWhere (#number, fromNumber)
                |> fetchOne
        toPhoneNumber <-
            query @PhoneNumber
                |> filterWhere (#number, toNumber)
                |> fetchOne

        twilioMessage
            |> set #fromId (get #id fromPhoneNumber)
            |> set #toId (get #id toPhoneNumber)
            |> createRecord

        renderPlain ""

validateCallbackSignature :: (?context :: ControllerContext) => IO ()
validateCallbackSignature = do
    case (getHeader "Host", getHeader "X-Twilio-Signature") of
        (Just host, Just receivedSignature) -> do
            let requestUrl = "https://" <> host <> getRequestPath
            let authToken = (\(Twilio.AuthToken token) -> encodeUtf8 token) Twilio.authToken
            let computedSignature = Twilio.callbackSignature authToken requestUrl allParams
            if receivedSignature == computedSignature
                then pure ()
                else do
                    Log.error $ "Invalid Twilio signature: " <> show receivedSignature <> " \\= " <> show computedSignature
                    respondAndExit $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request"
        _ -> do
            Log.error ("Twilio callback is missing Host or X-Twilio-Signature headers" :: Text)
            respondAndExit $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request"

buildIncomingTwilioMessage ::
    (?context :: ControllerContext) =>
    TwilioMessage ->
    TwilioMessage
buildIncomingTwilioMessage twilioMessage =
    twilioMessage
        |> set #apiVersion (param "ApiVersion")
        |> set #messageSid (param "MessageSid")
        |> set #accountSid (param "AccountSid")
        |> set #messagingServiceSid (paramOrNothing "MessagingServiceSid")
        |> set #status (param "SmsStatus")
        |> set #body (param "Body")
        |> set #numMedia (param "NumMedia")
