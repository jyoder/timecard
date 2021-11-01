module Application.Twilio.TwilioMessage (
    validate,
    send,
    delivered,
) where

import qualified Application.Audit.Entry as Audit.Entry
import Application.Service.Transaction (withTransactionOrSavepoint)
import Application.Service.Validation (validateAndCreate)
import qualified Application.Twilio.Client as Client
import Data.ByteString.UTF8 (toString)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Generated.Types
import IHP.ControllerPrelude

validate :: TwilioMessage -> TwilioMessage
validate twilioMessage =
    twilioMessage
        |> validateField #apiVersion nonEmpty
        |> validateField #messageSid nonEmpty
        |> validateField #accountSid nonEmpty
        |> validateField #status (isInList statuses)
        |> validateField #body nonEmpty
        |> validateField #numMedia (validateAny [isInList [0], isGreaterThan 0])

send ::
    ( ?context :: context
    , ConfigProvider context
    , ?modelContext :: ModelContext
    ) =>
    Maybe (Id User) ->
    PhoneNumber ->
    PhoneNumber ->
    Text ->
    IO TwilioMessage
send userId fromPhoneNumber toPhoneNumber body = do
    Client.Response {..} <-
        Client.sendPhoneMessage
            Client.config
            (get #number fromPhoneNumber)
            (get #number toPhoneNumber)
            body

    withTransactionOrSavepoint do
        twilioMessage <-
            newRecord @TwilioMessage
                |> set #apiVersion apiVersion
                |> set #messageSid messageSid
                |> set #accountSid accountSid
                |> set #fromId (get #id fromPhoneNumber)
                |> set #toId (get #id toPhoneNumber)
                |> set #status status
                |> set #body body
                |> set #numMedia numMedia
                |> validateAndCreate validate

        Audit.Entry.createMessageSent
            userId
            twilioMessage
            (get #number fromPhoneNumber)

        pure twilioMessage

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
