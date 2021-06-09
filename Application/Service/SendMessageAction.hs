module Application.Service.SendMessageAction where

import qualified Application.Service.Twilio as Twilio
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Generated.Types
import IHP.Fetch
import IHP.FrameworkConfig
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import Text.RawString.QQ (r)

data SendMessageAction'' = SendMessageAction''
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

instance FromRow SendMessageAction'' where
    fromRow =
        SendMessageAction''
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

fetchReadySendMessageActions ::
    ( ?modelContext :: ModelContext
    , ?context :: FrameworkConfig
    ) =>
    IO [SendMessageAction'']
fetchReadySendMessageActions = sqlQuery readySendMessageActionsQuery ()

readySendMessageActionsQuery :: Query
readySendMessageActionsQuery =
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

fetchFutureSendMessageActionsFor ::
    ( ?modelContext :: ModelContext
    ) =>
    Id Person ->
    IO [SendMessageAction'']
fetchFutureSendMessageActionsFor personId =
    sqlQuery futureSendMessageActionsForQuery (Only personId)

futureSendMessageActionsForQuery :: Query
futureSendMessageActionsForQuery =
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
    phone_contacts,
    phone_numbers from_phone_numbers,
    phone_numbers to_phone_numbers,
    send_message_actions,
    action_run_states,
    action_run_times
where
    phone_contacts.person_id = ? 
    and phone_contacts.phone_number_id = to_phone_numbers.id
    and to_phone_numbers.id = send_message_actions.to_id
    and from_phone_numbers.id = send_message_actions.from_id
    and send_message_actions.action_run_state_id = action_run_states.id
    and action_run_times.action_run_state_id = action_run_states.id
    and action_run_states.state = 'not_started'
    and action_run_times.runs_at > now()
order by
    action_run_times.runs_at asc;
|]

scheduleSendMessageAction ::
    (?modelContext :: ModelContext) =>
    Id PhoneNumber ->
    Id PhoneNumber ->
    Text ->
    UTCTime ->
    IO SendMessageAction
scheduleSendMessageAction fromId toId body runsAt =
    withTransaction do
        actionRunState <-
            newRecord @ActionRunState
                |> createRecord
        actionRunTime <-
            newRecord @ActionRunTime
                |> set #actionRunStateId (get #id actionRunState)
                |> set #runsAt runsAt
                |> createRecord
        newRecord @SendMessageAction
            |> set #actionRunStateId (get #id actionRunState)
            |> set #fromId fromId
            |> set #toId toId
            |> set #body body
            |> createRecord

performSendMessageAction ::
    ( ?modelContext :: ModelContext
    , ?context :: FrameworkConfig
    ) =>
    SendMessageAction'' ->
    IO ()
performSendMessageAction sendMessageAction = do
    Twilio.Response {..} <-
        Twilio.sendPhoneMessage
            Twilio.accountId
            Twilio.authToken
            Twilio.statusCallbackUrl
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