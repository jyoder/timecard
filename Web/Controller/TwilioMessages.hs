module Web.Controller.TwilioMessages where

import Web.Controller.Prelude
import Web.View.TwilioMessages.Edit
import Web.View.TwilioMessages.Index
import Web.View.TwilioMessages.New
import Web.View.TwilioMessages.Show

instance Controller TwilioMessagesController where
    action TwilioMessagesAction = do
        twilioMessages <- query @TwilioMessage |> fetch
        render IndexView {..}
    --
    action NewTwilioMessageAction = do
        let twilioMessage = newRecord
        render NewView {..}
    --
    action ShowTwilioMessageAction {twilioMessageId} = do
        twilioMessage <- fetch twilioMessageId
        render ShowView {..}
    --
    action EditTwilioMessageAction {twilioMessageId} = do
        twilioMessage <- fetch twilioMessageId
        render EditView {..}
    --
    action UpdateTwilioMessageAction {twilioMessageId} = do
        twilioMessage <- fetch twilioMessageId
        twilioMessage
            |> buildTwilioMessage
            |> ifValid \case
                Left twilioMessage -> render EditView {..}
                Right twilioMessage -> do
                    twilioMessage <- twilioMessage |> updateRecord
                    setSuccessMessage "TwilioMessage updated"
                    redirectTo EditTwilioMessageAction {..}
    --
    action CreateTwilioMessageAction = do
        let twilioMessage = newRecord @TwilioMessage
        twilioMessage
            |> buildTwilioMessage
            |> ifValid \case
                Left twilioMessage -> render NewView {..}
                Right twilioMessage -> do
                    twilioMessage <- twilioMessage |> createRecord
                    setSuccessMessage "TwilioMessage created"
                    redirectTo TwilioMessagesAction
    --
    action DeleteTwilioMessageAction {twilioMessageId} = do
        twilioMessage <- fetch twilioMessageId
        deleteRecord twilioMessage
        setSuccessMessage "TwilioMessage deleted"
        redirectTo TwilioMessagesAction

buildTwilioMessage ::
    (?context :: ControllerContext) =>
    TwilioMessage ->
    TwilioMessage
buildTwilioMessage twilioMessage =
    twilioMessage
        |> fill
            @[ "phoneMessageId"
             , "apiVersion"
             , "messageSid"
             , "accountSid"
             , "messagingServiceSid"
             , "messageStatus"
             , "fromNumber"
             , "toNumber"
             , "body"
             , "numMedia"
             ]
