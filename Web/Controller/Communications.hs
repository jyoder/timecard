{-# LANGUAGE PackageImports #-}

module Web.Controller.Communications where

import qualified Application.Service.Twilio as Twilio
import qualified Data.ByteString.Char8 as BS8
import qualified Data.TMap as TMap
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import qualified IHP.Log as Log
import Network.HTTP.Types (hContentType, status400)
import Network.Wai (responseLBS)
import Text.RawString.QQ (r)
import Web.Controller.Prelude
import Web.View.Communications.Index

instance Controller CommunicationsController where
    action CommunicationsAction {..} = autoRefresh do
        botId <- fetchBotId
        persons <- fetchPersonsExcluding botId
        selectedPerson <- fetch selectedPersonId
        toPhoneNumber <- fetchPhoneNumberFor selectedPersonId
        communications <- fetchCommunicationsBetween botId selectedPersonId
        let newMessage = newRecord @PhoneMessage |> set #toId (get #id toPhoneNumber)
        render IndexView {..}
    --
    action CreateOutgoingPhoneMessageAction = do
        let toPhoneNumberId = Id (param "toId")
        let body = param "body"
        botId <- fetchBotId
        toPerson <- fetchPersonFor toPhoneNumberId
        fromPhoneNumber <- fetchPhoneNumberFor botId
        toPhoneNumber <- fetchOne toPhoneNumberId
        Twilio.sendPhoneMessage
            twilioAccountId
            twilioAuthToken
            twilioStatusCallbackUrl
            (get #number fromPhoneNumber)
            (get #number toPhoneNumber)
            body
        now <- getCurrentTime
        withTransaction do
            newRecord @PhoneMessage
                |> set #fromId (get #id fromPhoneNumber)
                |> set #toId toPhoneNumberId
                |> setJust #sentAt now
                |> set #body body
                |> createRecord
        redirectTo $ CommunicationsAction $ get #id toPerson
    --
    action UpdateOutgoingPhoneMessageAction = do
        validateCallbackSignature
        renderPlain ""
    --
    action CreateIncomingPhoneMessageAction = do
        validateCallbackSignature
        let twilioMessage = buildIncomingTwilioMessage newRecord
        let fromNumber = get #fromNumber twilioMessage
        let toNumber = get #toNumber twilioMessage
        let body = get #body twilioMessage
        fromPhoneNumber <- query @PhoneNumber |> filterWhere (#number, fromNumber) |> fetchOne
        toPhoneNumber <- query @PhoneNumber |> filterWhere (#number, toNumber) |> fetchOne
        now <- getCurrentTime
        withTransaction do
            phoneMessage <-
                newRecord @PhoneMessage
                    |> set #fromId (get #id fromPhoneNumber)
                    |> set #toId (get #id toPhoneNumber)
                    |> setJust #sentAt now
                    |> set #body body
                    |> createRecord
            twilioMessage
                |> set #phoneMessageId (get #id phoneMessage)
                |> createRecord
        renderPlain ""

fetchPersonsExcluding :: (?modelContext :: ModelContext) => Id Person -> IO [Person]
fetchPersonsExcluding idToExclude = do
    persons <- query @Person |> orderByAsc #lastName |> fetch
    filter (\person -> get #id person /= idToExclude) persons |> pure

fetchPhoneNumberFor :: (?modelContext :: ModelContext) => Id Person -> IO PhoneNumber
fetchPhoneNumberFor personId = do
    phoneContact <-
        query @PhoneContact
            |> filterWhere (#personId, personId)
            |> fetchOne
    fetchOne (get #phoneNumberId phoneContact)

fetchPersonFor :: (?modelContext :: ModelContext) => Id PhoneNumber -> IO Person
fetchPersonFor phoneNumberId = do
    phoneContact <-
        query @PhoneContact
            |> filterWhere (#phoneNumberId, phoneNumberId)
            |> fetchOne
    fetchOne (get #personId phoneContact)

fetchBotId :: (?modelContext :: ModelContext) => IO (Id Person)
fetchBotId = get #id <$> fetchBot

fetchBot :: (?modelContext :: ModelContext) => IO Person
fetchBot = query @Person |> filterWhere (#goesBy, botName) |> fetchOne

botName :: Text
botName = "Tim the Bot"

fetchCommunicationsBetween :: (?modelContext :: ModelContext) => Id Person -> Id Person -> IO [Communication]
fetchCommunicationsBetween personIdA personIdB = do
    trackTableRead "phone_messages"
    sqlQuery communicationsQuery (personIdA, personIdB)

validateCallbackSignature :: (?context :: ControllerContext) => IO ()
validateCallbackSignature = do
    case (getHeader "Host", getHeader "X-Twilio-Signature") of
        (Just host, Just receivedSignature) -> do
            let requestUrl = "https://" <> host <> getRequestPath
            let authToken = (\(Twilio.AuthToken token) -> encodeUtf8 token) twilioAuthToken
            let computedSignature = Twilio.callbackSignature authToken requestUrl allParams
            if receivedSignature == computedSignature
                then pure ()
                else do
                    Log.error $ "Invalid Twilio signature: " <> show receivedSignature <> " \\= " <> show computedSignature
                    respondAndExit $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request"
        _ -> do
            Log.info ("Twilio callback is missing Host or X-Twilio-Signature headers" :: Text)
            respondAndExit $ responseLBS status400 [(hContentType, "text/plain")] "Bad Request"

twilioAccountId :: (?context :: ControllerContext) => Twilio.AccountId
twilioAccountId =
    ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TMap.lookup @Twilio.AccountId
        |> fromMaybe (error "Could not find Twilio.AccountId in config")

twilioAuthToken :: (?context :: ControllerContext) => Twilio.AuthToken
twilioAuthToken =
    ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TMap.lookup @Twilio.AuthToken
        |> fromMaybe (error "Could not find Twilio.AuthToken in config")

twilioStatusCallbackUrl :: (?context :: ControllerContext) => Twilio.StatusCallbackUrl
twilioStatusCallbackUrl =
    ?context
        |> getFrameworkConfig
        |> get #appConfig
        |> TMap.lookup @Twilio.StatusCallbackUrl
        |> fromMaybe (error "Could not find Twilio.StatusCallbackUrl in config")

buildIncomingTwilioMessage ::
    (?context :: ControllerContext) =>
    TwilioMessage ->
    TwilioMessage
buildIncomingTwilioMessage twilioMessage =
    twilioMessage
        |> set #apiVersion (param "ApiVersion")
        |> set #messageSid (param "MessageSid")
        |> set #accountSid (param "AccountSid")
        |> set #messagingServiceSid (paramOrNothing "MessagingServiceSid")
        |> set #messageStatus (paramOrNothing "MessageStatus")
        |> set #fromNumber (param "From")
        |> set #toNumber (param "To")
        |> set #body (param "Body")
        |> set #numMedia (param "NumMedia")

communicationsQuery :: Query
communicationsQuery =
    [r|
select
    persons_a.goes_by name_a,
    persons_b.goes_by name_b,
    (phone_messages.from_id = phone_numbers_a.id) is_from_person_a,
    phone_messages.created_at,
    phone_messages.sent_at,
    phone_messages.body
from
    persons persons_a,
    phone_contacts phone_contacts_a,
    phone_numbers phone_numbers_a,
    persons persons_b,
    phone_contacts phone_contacts_b,
    phone_numbers phone_numbers_b,
    phone_messages
where
    persons_a.id = ?
    and phone_contacts_a.person_id = persons_a.id 
    and phone_contacts_a.phone_number_id = phone_numbers_a.id 
    and persons_b.id = ?
    and phone_contacts_b.person_id = persons_b.id 
    and phone_contacts_b.phone_number_id = phone_numbers_b.id 
    and ((phone_messages.from_id = phone_numbers_a.id 
          and phone_messages.to_id = phone_numbers_b.id)
          or
          (phone_messages.from_id = phone_numbers_b.id 
           and phone_messages.to_id = phone_numbers_a.id))
order by
    phone_messages.created_at asc;
|]

instance FromRow Communication where
    fromRow = Communication <$> field <*> field <*> field <*> field <*> field <*> field
