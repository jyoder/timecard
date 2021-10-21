module Application.Twilio.Query (
    Row (..),
    Status (..),
    EntityType (..),
    fetchById,
    fetchByPeople,
) where

import Data.ByteString.UTF8 (toString)
import "string-interpolate" Data.String.Interpolate (i)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import qualified Generated.Types as Types
import IHP.ControllerPrelude

data Row = Row
    { id :: !(Id Types.TwilioMessage)
    , fromPhoneNumber :: !Text
    , fromFirstName :: !Text
    , fromLastName :: !Text
    , toPhoneNumber :: !Text
    , toFirstName :: !Text
    , toLastName :: !Text
    , createdAt :: !UTCTime
    , status :: !Status
    , body :: !(Maybe Text)
    , entityType :: !(Maybe EntityType)
    , entityStart :: !(Maybe Int)
    , entityEnd :: !(Maybe Int)
    , entityConfidence :: !(Maybe Double)
    }
    deriving (Eq, Show)

data Status
    = Accepted
    | Scheduled
    | Queued
    | Sending
    | Sent
    | Receiving
    | Received
    | Delivered
    | Undelivered
    | Failed
    | Read
    deriving (Eq, Show)

instance FromRow Row where
    fromRow =
        Row
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

instance FromField Status where
    fromField field maybeData =
        case maybeData of
            Just "accepted" -> pure Accepted
            Just "scheduled" -> pure Scheduled
            Just "queued" -> pure Queued
            Just "sending" -> pure Sending
            Just "sent" -> pure Sent
            Just "receiving" -> pure Receiving
            Just "received" -> pure Received
            Just "delivered" -> pure Delivered
            Just "undelivered" -> pure Undelivered
            Just "failed" -> pure Failed
            Just "read" -> pure Read
            Just _data -> returnError ConversionFailed field (toString _data)
            Nothing -> returnError UnexpectedNull field ""

data EntityType
    = JobName
    | HoursWorked
    | ClockedInAt
    | ClockedOutAt
    | TimeOnTask
    | WorkDone
    | Unrecognized
    deriving (Eq, Show)

instance FromField EntityType where
    fromField field maybeData =
        case maybeData of
            Just "job_name" -> pure JobName
            Just "hours_worked" -> pure HoursWorked
            Just "clocked_in_at" -> pure ClockedInAt
            Just "clocked_out_at" -> pure ClockedOutAt
            Just "time_on_task" -> pure TimeOnTask
            Just "work_done" -> pure WorkDone
            Just _data -> pure Unrecognized
            Nothing -> returnError UnexpectedNull field ""

fetchById ::
    (?modelContext :: ModelContext) =>
    Id Types.TwilioMessage ->
    IO [Row]
fetchById twilioMessageId = do
    trackTableRead "twilio_messages"
    sqlQuery messageQuery (Only twilioMessageId)

fetchByPeople ::
    (?modelContext :: ModelContext) =>
    Id Types.Person ->
    Id Types.Person ->
    IO [Row]
fetchByPeople personIdA personIdB = do
    trackTableRead "twilio_messages"
    trackTableRead "twilio_message_entities"
    sqlQuery messagesQuery2 (personIdA, personIdB)

messageQuery :: Query
messageQuery =
    [i|
        with
            #{messageDetailsIdCte},
            #{predictionsCte}
        #{mainQuery}
    |]

messagesQuery2 :: Query
messagesQuery2 =
    [i|
        with
            #{messageDetailsPeopleCte},
            #{predictionsCte}
        #{mainQuery}
    |]

messageDetailsIdCte :: Text
messageDetailsIdCte =
    [i|
        message_details as (
            select
                twilio_messages.id id,
                phone_numbers_a.number from_phone_number,
                people_a.first_name from_first_name,
                people_a.last_name from_last_name,
                phone_numbers_b.number to_phone_number,
                people_b.first_name to_first_name,
                people_b.last_name to_last_name,
                twilio_messages.body body,
                twilio_messages.status status,
                twilio_messages.created_at created_at
            from
                people people_a,
                phone_contacts phone_contacts_a,
                phone_numbers phone_numbers_a,
                people people_b,
                phone_contacts phone_contacts_b,
                phone_numbers phone_numbers_b,
                twilio_messages
            where
                twilio_messages.id = ?
                and phone_contacts_a.person_id = people_a.id
                and phone_contacts_a.phone_number_id = phone_numbers_a.id
                and phone_contacts_b.person_id = people_b.id
                and phone_contacts_b.phone_number_id = phone_numbers_b.id
                and twilio_messages.from_id = phone_numbers_a.id
                and twilio_messages.to_id = phone_numbers_b.id
            order by
                twilio_messages.created_at desc
        )
    |]

messageDetailsPeopleCte :: Text
messageDetailsPeopleCte =
    [i|
        message_details as (
            select
                twilio_messages.id id,
                (case when twilio_messages.from_id = phone_numbers_a.id
                    then phone_numbers_a.number
                    else phone_numbers_b.number
                end) from_phone_number,
                (case when twilio_messages.from_id = phone_numbers_a.id
                    then people_a.first_name
                    else people_b.first_name
                end) from_first_name,
                (case when twilio_messages.from_id = phone_numbers_a.id
                    then people_a.last_name
                    else people_b.last_name
                end) from_last_name,
                (case when twilio_messages.to_id = phone_numbers_a.id
                    then phone_numbers_a.number
                    else phone_numbers_b.number
                end) to_phone_number,
                (case when twilio_messages.to_id = phone_numbers_a.id
                    then people_a.first_name
                    else people_b.first_name
                end) to_first_name,
                (case when twilio_messages.to_id = phone_numbers_a.id
                    then people_a.last_name
                    else people_b.last_name
                end) to_last_name,
                twilio_messages.body body,
                twilio_messages.status status,
                twilio_messages.created_at created_at
            from
                people people_a,
                phone_contacts phone_contacts_a,
                phone_numbers phone_numbers_a,
                people people_b,
                phone_contacts phone_contacts_b,
                phone_numbers phone_numbers_b,
                twilio_messages
            where
                people_a.id = ?
                and phone_contacts_a.person_id = people_a.id
                and phone_contacts_a.phone_number_id = phone_numbers_a.id
                and people_b.id = ?
                and phone_contacts_b.person_id = people_b.id
                and phone_contacts_b.phone_number_id = phone_numbers_b.id
                and ((twilio_messages.from_id = phone_numbers_a.id
                    and twilio_messages.to_id = phone_numbers_b.id)
                    or
                    (twilio_messages.from_id = phone_numbers_b.id
                    and twilio_messages.to_id = phone_numbers_a.id))
            order by
                twilio_messages.created_at desc
        )
    |]

predictionsCte :: Text
predictionsCte =
    [i|
        predictions as (
            select
                message_details.id id,
                vertex_ai_entity_predictions.display_name,
                vertex_ai_entity_predictions.segment_start_offset,
                vertex_ai_entity_predictions.segment_end_offset,
                vertex_ai_entity_predictions.confidence,
                vertex_ai_entity_predictions.deployed_model_id,
                row_number()
                    over (
                        partition by message_details.id 
                    order by 
                        vertex_ai_entity_predictions.segment_start_offset asc) as row
            from
                message_details
                left join
                    twilio_message_entities on
                        (twilio_message_entities.twilio_message_id = message_details.id)
                left join
                    vertex_ai_entity_predictions on
                        (vertex_ai_entity_predictions.id = twilio_message_entities.vertex_ai_entity_prediction_id)
            order by
                vertex_ai_entity_predictions.segment_start_offset asc
        )
    |]

mainQuery :: Text
mainQuery =
    [i|
        select
            message_details.id,
            message_details.from_phone_number,
            message_details.from_first_name,
            message_details.from_last_name,
            message_details.to_phone_number,
            message_details.to_first_name,
            message_details.to_last_name,
            message_details.created_at,
            message_details.status,
            (case when predictions.row = 1 then message_details.body else null end),
            predictions.display_name,
            predictions.segment_start_offset,
            predictions.segment_end_offset,
            predictions.confidence
        from
            message_details,
            predictions
        where
            message_details.id = predictions.id
        order by
            message_details.created_at asc,
            message_details.id asc,
            predictions.row asc
    |]
