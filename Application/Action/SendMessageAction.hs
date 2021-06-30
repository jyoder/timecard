module Application.Action.SendMessageAction (
    T (..),
    validate,
    fetchReady,
    fetchAfterByPhoneNumber,
    schedule,
    perform,
) where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.ActionRunTime as ActionRunTime
import Application.Service.Validation (validateAndCreate)
import qualified Application.Twilio.TwilioClient as TwilioClient
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Generated.Types
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.ModelSupport
import IHP.Prelude
import IHP.ValidationSupport.ValidateField
import Text.RawString.QQ (r)

data T = T
    { id :: !(Id SendMessageAction)
    , actionRunStateId :: !(Id ActionRunState)
    , state :: !Text
    , runsAt :: !UTCTime
    , body :: !Text
    , fromId :: !(Id PhoneNumber)
    , fromNumber :: !Text
    , toId :: !(Id PhoneNumber)
    , toNumber :: !Text
    }

instance FromRow T where
    fromRow =
        T
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

validate :: SendMessageAction -> SendMessageAction
validate sendMessageAction =
    sendMessageAction
        |> validateField #body nonEmpty

fetchReady :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO [T]
fetchReady = do
    trackTableRead "send_message_actions"
    trackTableRead "action_run_times"
    trackTableRead "action_run_states"
    sqlQuery fetchReadyQuery ()

fetchReadyQuery :: Query
fetchReadyQuery =
    [r|
select
    send_message_actions.id,
    action_run_states.id,
    action_run_states.state,
    action_run_times.runs_at,
    send_message_actions.body,
    send_message_actions.from_id,
    from_phone_numbers.number,
    send_message_actions.to_id,
    to_phone_numbers.number
from
    send_message_actions,
    phone_numbers from_phone_numbers,
    phone_numbers to_phone_numbers,
    action_run_states,
    action_run_times
where
    send_message_actions.action_run_state_id = action_run_states.id
    and send_message_actions.from_id = from_phone_numbers.id
    and send_message_actions.to_id = to_phone_numbers.id
    and action_run_times.action_run_state_id = action_run_states.id
    and action_run_states.state = 'not_started'
    and action_run_times.runs_at <= now()
order by
    action_run_times.runs_at asc;
|]

fetchAfterByPhoneNumber ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    Id PhoneNumber ->
    IO [T]
fetchAfterByPhoneNumber time toPhoneNumberId = do
    trackTableRead "send_message_actions"
    trackTableRead "action_run_times"
    trackTableRead "action_run_states"
    sqlQuery fetchFutureByPhoneNumberQuery (toPhoneNumberId, time)

fetchFutureByPhoneNumberQuery :: Query
fetchFutureByPhoneNumberQuery =
    [r|
select
    send_message_actions.id,
    action_run_states.id,
    action_run_states.state,
    action_run_times.runs_at,
    send_message_actions.body,
    send_message_actions.from_id,
    from_phone_numbers.number,
    send_message_actions.to_id,
    to_phone_numbers.number
from
    phone_numbers from_phone_numbers,
    phone_numbers to_phone_numbers,
    send_message_actions,
    action_run_states,
    action_run_times
where
    to_phone_numbers.id = ?
    and to_phone_numbers.id = send_message_actions.to_id
    and from_phone_numbers.id = send_message_actions.from_id
    and send_message_actions.action_run_state_id = action_run_states.id
    and action_run_times.action_run_state_id = action_run_states.id
    and action_run_states.state = 'not_started'
    and action_run_times.runs_at > ?
order by
    action_run_times.runs_at asc;
|]

schedule ::
    (?modelContext :: ModelContext) =>
    Id PhoneNumber ->
    Id PhoneNumber ->
    Text ->
    UTCTime ->
    IO SendMessageAction
schedule fromId toId body runsAt = do
    actionRunState <-
        newRecord @ActionRunState
            |> validateAndCreate ActionRunState.validate
    actionRunTime <-
        newRecord @ActionRunTime
            |> set #actionRunStateId (get #id actionRunState)
            |> set #runsAt runsAt
            |> validateAndCreate ActionRunTime.validate
    newRecord @SendMessageAction
        |> set #actionRunStateId (get #id actionRunState)
        |> set #fromId fromId
        |> set #toId toId
        |> set #body body
        |> validateAndCreate validate

perform :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => T -> IO ()
perform sendMessageAction = do
    TwilioClient.Response {..} <-
        TwilioClient.sendPhoneMessage
            TwilioClient.accountId
            TwilioClient.authToken
            TwilioClient.statusCallbackUrl
            (get #fromNumber sendMessageAction)
            (get #toNumber sendMessageAction)
            (get #body sendMessageAction)

    newRecord @TwilioMessage
        |> set #apiVersion apiVersion
        |> set #messageSid messageSid
        |> set #accountSid accountSid
        |> set #fromId (get #fromId sendMessageAction)
        |> set #toId (get #toId sendMessageAction)
        |> set #status status
        |> set #body body
        |> set #numMedia numMedia
        |> createRecord

    pure ()
