module Application.Twilio.Query where

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
    , body :: !Text
    }

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
    deriving (Show)

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

fetchByPeople ::
    (?modelContext :: ModelContext) =>
    Id Types.Person ->
    Id Types.Person ->
    IO [Row]
fetchByPeople personIdA personIdB = do
    trackTableRead "twilio_messages"
    sqlQuery messagesQuery (personIdA, personIdB)

messagesQuery :: Query
messagesQuery =
    [i|
select
    twilio_messages.id,
    (case when twilio_messages.from_id = phone_numbers_a.id
          then phone_number_a.number
          else phone_number_b.number
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
          then phone_number_a.number
          else phone_number_b.number
    end) to_phone_number,
    (case when twilio_messages.to_id = phone_numbers_a.id
          then people_a.first_name
          else people_b.first_name
    end) to_first_name,
    (case when twilio_messages.to_id = phone_numbers_a.id
          then people_a.last_name
          else people_b.last_name
    end) to_last_name,
    twilio_messages.created_at,
    twilio_messages.status,
    twilio_messages.body
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
    twilio_messages.created_at asc;
|]
