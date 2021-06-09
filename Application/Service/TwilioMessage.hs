module Application.Service.TwilioMessage (
    T (..),
    Status (..),
    validate,
    fetchByPeople,
    delivered,
) where

import Data.ByteString.UTF8 (toString)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import IHP.ValidationSupport.ValidateField
import Text.RawString.QQ (r)

data T = T
    { id :: !(Id TwilioMessage)
    , fromName :: !Text
    , toName :: !Text
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

instance FromRow T where
    fromRow =
        T
            <$> field
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

validate :: TwilioMessage -> TwilioMessage
validate twilioMessage =
    twilioMessage
        |> validateField #apiVersion nonEmpty
        |> validateField #messageSid nonEmpty
        |> validateField #accountSid nonEmpty
        |> validateField #status (isInList statuses)
        |> validateField #body nonEmpty
        |> validateField #numMedia (validateAny [isInList [0], isGreaterThan 0])

fetchByPeople ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    Id Person ->
    IO [T]
fetchByPeople personIdA personIdB = do
    trackTableRead "twilio_messages"
    sqlQuery messagesQuery (personIdA, personIdB)

messagesQuery :: Query
messagesQuery =
    [r|
select
    twilio_messages.id,
    (case when twilio_messages.from_id = phone_numbers_a.id
          then people_a.goes_by
          else people_b.goes_by
    end) from_name,
    (case when twilio_messages.to_id = phone_numbers_a.id
          then people_a.goes_by
          else people_b.goes_by
    end) to_name,
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

statuses :: [Text]
statuses =
    [ accepted
    , scheduled
    , queued
    , sending
    , sent
    , receiving
    , received
    , delivered
    , undelivered
    , failed
    , read
    ]

accepted :: Text
accepted = "accepted"

scheduled :: Text
scheduled = "scheduled"

queued :: Text
queued = "queued"

sending :: Text
sending = "sending"

sent :: Text
sent = "sent"

receiving :: Text
receiving = "receiving"

received :: Text
received = "received"

delivered :: Text
delivered = "delivered"

undelivered :: Text
undelivered = "undelivered"

failed :: Text
failed = "failed"

read :: Text
read = "read"
