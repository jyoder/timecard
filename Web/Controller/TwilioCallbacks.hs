module Web.Controller.TwilioCallbacks where

import qualified Application.Brain.Process as Brain.Process
import Application.Service.Transaction (withTransactionOrSavepoint)
import Application.Service.Validation (validateAndUpdate)
import qualified Application.Twilio.TwilioClient as TwilioClient
import qualified Application.Twilio.TwilioMessage as TwilioMessage
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

        withTransactionOrSavepoint do
            twilioMessage <-
                query @TwilioMessage
                    |> filterWhere (#messageSid, messageSid)
                    |> fetchOne

            if messageStatus == TwilioMessage.delivered
                then
                    twilioMessage
                        |> set #status messageStatus
                        |> validateAndUpdate TwilioMessage.validate
                        >> pure ()
                else pure ()

        renderPlain ""
    --
    action CreateIncomingPhoneMessageAction = do
        validateCallbackSignature

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

        newRecord @TwilioMessage
            |> buildIncomingTwilioMessage
                (get #id fromPhoneNumber)
                (get #id toPhoneNumber)
            |> ifValid \case
                Left twilioMessage -> do
                    Log.error ("Failed to save incoming message: " <> show twilioMessage)
                    pure ()
                Right twilioMessage -> do
                    createRecord twilioMessage
                    pure ()

        Brain.Process.processState $ get #id fromPhoneNumber

        renderPlain ""

validateCallbackSignature :: (?context :: ControllerContext) => IO ()
validateCallbackSignature = do
    case (getHeader "Host", getHeader "X-Twilio-Signature") of
        (Just host, Just receivedSignature) -> do
            let requestUrl = "https://" <> host <> getRequestPath
            let authToken = encodeUtf8 (get #authToken TwilioClient.config)
            let computedSignature = TwilioClient.callbackSignature authToken requestUrl allParams
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
    Id PhoneNumber ->
    Id PhoneNumber ->
    TwilioMessage ->
    TwilioMessage
buildIncomingTwilioMessage fromId toId twilioMessage =
    twilioMessage
        |> set #fromId fromId
        |> set #toId toId
        |> set #apiVersion (param "ApiVersion")
        |> set #messageSid (param "MessageSid")
        |> set #accountSid (param "AccountSid")
        |> set #messagingServiceSid (paramOrNothing "MessagingServiceSid")
        |> set #status (param "SmsStatus")
        |> set #body (param "Body")
        |> set #numMedia (param "NumMedia")
        |> TwilioMessage.validate
