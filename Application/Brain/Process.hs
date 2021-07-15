module Application.Brain.Process (
    processState,
    suspendSendMessageActionsAfter,
) where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.ActionRunTime as ActionRunTime
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Base.PhoneNumber as PhoneNumber
import Generated.Types
import IHP.ControllerPrelude
import IHP.Log as Log
import IHP.ModelSupport

processState :: (?modelContext :: ModelContext) => UTCTime -> Id PhoneNumber -> IO ()
processState now phoneNumberId = do
    twilioMessages <-
        query @TwilioMessage
            |> filterWhere (#fromId, phoneNumberId)
            |> orderByDesc #createdAt
            |> limit 1
            |> fetch

    case twilioMessages of
        twilioMessage : messages ->
            suspendSendMessageActionsAfter
                phoneNumberId
                now
                (get #createdAt twilioMessage)
        [] -> pure ()

suspendSendMessageActionsAfter ::
    (?modelContext :: ModelContext) =>
    Id PhoneNumber ->
    UTCTime ->
    UTCTime ->
    IO ()
suspendSendMessageActionsAfter phoneNumberId now time = do
    sendMessageActions <-
        SendMessageAction.fetchFutureCreatedBeforeByPhoneNumber
            now
            time
            phoneNumberId

    mapM_
        ( \action -> do
            actionRunState <- fetch $ get #actionRunStateId action
            ActionRunState.updateSuspended actionRunState
        )
        sendMessageActions