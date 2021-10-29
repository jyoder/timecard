module Application.Action.SendMessageAction (
    T (..),
    validate,
    fetchReadyToRun,
    fetchNotStartedOrSuspendedByPhoneNumber,
    fetchNotStartedCreatedBeforeByPhoneNumber,
    schedule,
    perform,
) where

import qualified Application.Action.ActionRunState as ActionRunState
import Application.Service.Transaction (withTransactionOrSavepoint)
import Application.Service.Validation (validateAndCreate)
import qualified Application.Twilio.TwilioMessage as TwilioMessage
import "string-interpolate" Data.String.Interpolate (i)
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Generated.Types
import IHP.Fetch (fetch)
import IHP.FrameworkConfig (FrameworkConfig)
import IHP.ModelSupport
import IHP.Prelude
import IHP.ValidationSupport.ValidateField

-- TODO: consider dropping fromNumber and toNumber from this structure
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
    deriving (Eq, Show)

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

data WhereCondition
    = ReadyToRunCondition
    | NotStartedOrSuspendedByPhoneNumberCondition
    | NotStartedCreatedBeforeByPhoneNumberCondition

validate :: SendMessageAction -> SendMessageAction
validate sendMessageAction =
    sendMessageAction
        |> validateField #body nonEmpty

fetchReadyToRun ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    IO [T]
fetchReadyToRun now = do
    trackTableReads
    sqlQuery (query ReadyToRunCondition) (Only now)

fetchNotStartedOrSuspendedByPhoneNumber ::
    (?modelContext :: ModelContext) =>
    Id PhoneNumber ->
    IO [T]
fetchNotStartedOrSuspendedByPhoneNumber toPhoneNumberId = do
    trackTableReads
    sqlQuery (query NotStartedOrSuspendedByPhoneNumberCondition) (Only toPhoneNumberId)

fetchNotStartedCreatedBeforeByPhoneNumber ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    Id PhoneNumber ->
    IO [T]
fetchNotStartedCreatedBeforeByPhoneNumber time fromPhoneNumberId = do
    trackTableReads
    sqlQuery (query NotStartedCreatedBeforeByPhoneNumberCondition) (fromPhoneNumberId, time)

schedule ::
    (?modelContext :: ModelContext) =>
    Id PhoneNumber ->
    Id PhoneNumber ->
    Text ->
    UTCTime ->
    IO SendMessageAction
schedule fromId toId body runsAt = do
    withTransactionOrSavepoint do
        actionRunState <-
            newRecord @ActionRunState
                |> set #runsAt runsAt
                |> validateAndCreate ActionRunState.validate
        newRecord @SendMessageAction
            |> set #actionRunStateId (get #id actionRunState)
            |> set #fromId fromId
            |> set #toId toId
            |> set #body body
            |> validateAndCreate validate

perform :: (?modelContext :: ModelContext, ?context :: FrameworkConfig) => T -> IO TwilioMessage
perform sendMessageAction = do
    fromPhoneNumber <- fetch $ get #fromId sendMessageAction
    toPhoneNumber <- fetch $ get #toId sendMessageAction
    TwilioMessage.send
        Nothing
        fromPhoneNumber
        toPhoneNumber
        (get #body sendMessageAction)

trackTableReads :: (?modelContext :: ModelContext) => IO ()
trackTableReads = do
    trackTableRead "send_message_actions"
    trackTableRead "action_run_states"

query :: WhereCondition -> Query
query whereCondition =
    [i|
        select
            send_message_actions.id,
            action_run_states.id,
            action_run_states.state,
            action_run_states.runs_at,
            send_message_actions.body,
            send_message_actions.from_id,
            from_phone_numbers.number,
            send_message_actions.to_id,
            to_phone_numbers.number
        from
            phone_numbers from_phone_numbers,
            phone_numbers to_phone_numbers,
            send_message_actions,
            action_run_states
        where
            to_phone_numbers.id = send_message_actions.to_id
            and from_phone_numbers.id = send_message_actions.from_id
            and send_message_actions.action_run_state_id = action_run_states.id
            and #{whereConditionSql whereCondition}
        order by
            action_run_states.runs_at asc;
    |]

whereConditionSql :: WhereCondition -> Text
whereConditionSql ReadyToRunCondition =
    [i|
        action_run_states.state = 'not_started'
        and action_run_states.runs_at <= ?
    |]
whereConditionSql NotStartedOrSuspendedByPhoneNumberCondition =
    [i|
        to_phone_numbers.id = ?
        and (
            action_run_states.state = 'not_started' 
            or action_run_states.state = 'suspended'
        )
    |]
whereConditionSql NotStartedCreatedBeforeByPhoneNumberCondition =
    [i|
        to_phone_numbers.id = ?
        and action_run_states.state = 'not_started'
        and send_message_actions.created_at <= ?
    |]
