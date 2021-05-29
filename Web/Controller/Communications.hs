{-# LANGUAGE PackageImports #-}

module Web.Controller.Communications where

import qualified Application.Service.Twilio as Twilio
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.UTF8 (toString)
import qualified Data.TMap as TMap
import Data.Text (strip)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import qualified IHP.Log as Log
import Network.HTTP.Types (hContentType, status400)
import Network.Wai (responseLBS)
import Text.RawString.QQ (r)
import Web.Controller.Prelude
import Web.View.Communications.Index

instance Controller CommunicationsController where
    action CommunicationsAction = autoRefresh do
        ensureIsUser
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        let personSelection = NoPersonSelected
        render IndexView {people, personSelection}
    --
    action PersonSelectionAction {..} = autoRefresh do
        ensureIsUser
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        selectedPerson <- fetch selectedPersonId
        messages <- fetchMessagesBetween botId selectedPersonId
        toPhoneNumberId <- get #phoneNumberId <$> query @PhoneContact |> filterWhere (#personId, selectedPersonId) |> fetchOne
        toPhoneNumber <- fetch toPhoneNumberId
        timecardEntries <- query @TimecardEntry |> filterWhere (#personId, get #id selectedPerson) |> fetch
        let newMessage = newRecord @TwilioMessage
        let personActivity = SendingMessage {timecardEntries}
        let personSelection = PersonSelected {selectedPerson, messages, toPhoneNumber, newMessage, personActivity}
        render IndexView {people, personSelection}
    --
    action NewTimecardEntryAction {..} = autoRefresh do
        ensureIsUser
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        selectedPerson <- fetch selectedPersonId
        messages <- fetchMessagesBetween botId selectedPersonId
        toPhoneNumberId <- get #phoneNumberId <$> query @PhoneContact |> filterWhere (#personId, selectedPersonId) |> fetchOne
        toPhoneNumber <- fetch toPhoneNumberId
        now <- getCurrentTime
        let selectedMessageIds = paramOrDefault @[Id TwilioMessage] [] "selectedMessageIds"
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        let timecardDate = maybe now (get #createdAt) (head selectedMessages)
        let timecardEntry = newRecord @TimecardEntry |> buildNewTimecardEntry timecardDate
        let timecardActivity = CreatingEntry
        let newMessage = newRecord @TwilioMessage
        let personActivity = WorkingOnTimecardEntry {timecardEntry, selectedMessages, timecardActivity}
        let personSelection = PersonSelected {selectedPerson, messages, toPhoneNumber, newMessage, personActivity}
        if null selectedMessageIds
            then redirectTo PersonSelectionAction {selectedPersonId}
            else render IndexView {people, personSelection}
    --
    action EditTimecardEntryAction {..} = autoRefresh do
        ensureIsUser
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        selectedPerson <- fetch selectedPersonId
        messages <- fetchMessagesBetween botId selectedPersonId
        toPhoneNumberId <- get #phoneNumberId <$> query @PhoneContact |> filterWhere (#personId, selectedPersonId) |> fetchOne
        toPhoneNumber <- fetch toPhoneNumberId
        timecardEntry <- fetch timecardEntryId
        timecardEntryMessages <- query @TimecardEntryMessage |> filterWhere (#timecardEntryId, get #id timecardEntry) |> fetch
        let selectedMessageIds = map (get #twilioMessageId) timecardEntryMessages
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        let timecardActivity = EditingEntry
        let newMessage = newRecord @TwilioMessage
        let personActivity = WorkingOnTimecardEntry {timecardEntry, selectedMessages, timecardActivity}
        let personSelection = PersonSelected {selectedPerson, messages, toPhoneNumber, newMessage, personActivity}
        if null selectedMessageIds
            then redirectTo PersonSelectionAction {selectedPersonId}
            else render IndexView {people, personSelection}
    --
    action EditModifiedTimecardEntryAction {..} = autoRefresh do
        ensureIsUser
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        selectedPerson <- fetch selectedPersonId
        messages <- fetchMessagesBetween botId selectedPersonId
        toPhoneNumberId <- get #phoneNumberId <$> query @PhoneContact |> filterWhere (#personId, selectedPersonId) |> fetchOne
        toPhoneNumber <- fetch toPhoneNumberId
        timecardEntry <- fetch timecardEntryId
        let selectedMessageIds = paramOrDefault @[Id TwilioMessage] [] "selectedMessageIds"
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        let timecardActivity = EditingModifiedEntry
        let newMessage = newRecord @TwilioMessage
        let personActivity = WorkingOnTimecardEntry {timecardEntry, selectedMessages, timecardActivity}
        let personSelection = PersonSelected {selectedPerson, messages, toPhoneNumber, newMessage, personActivity}
        if null selectedMessageIds
            then redirectTo PersonSelectionAction {selectedPersonId}
            else render IndexView {people, personSelection}
    --
    action CreateTimecardEntryAction = do
        ensureIsUser
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        let selectedMessageIds = param @[Id TwilioMessage] "selectedMessageIds"
        newRecord @TimecardEntry
            |> buildTimecardEntry
            |> set #personId selectedPersonId
            |> ifValid \case
                Left timecardEntry -> do
                    botId <- fetchBotId
                    people <- fetchPeopleExcluding botId
                    messages <- fetchMessagesBetween botId selectedPersonId
                    toPhoneNumberId <- get #phoneNumberId <$> query @PhoneContact |> filterWhere (#personId, selectedPersonId) |> fetchOne
                    toPhoneNumber <- fetch toPhoneNumberId
                    selectedPerson <- fetch selectedPersonId
                    let selectedMessages = findSelectedMessages messages selectedMessageIds
                    let timecardActivity = CreatingEntry
                    let newMessage = newRecord @TwilioMessage
                    let personActivity = WorkingOnTimecardEntry {timecardEntry, selectedMessages, timecardActivity}
                    let personSelection = PersonSelected {selectedPerson, messages, toPhoneNumber, newMessage, personActivity}
                    render IndexView {people, personSelection}
                Right timecardEntry -> do
                    withTransaction do
                        timecardEntry <- createRecord timecardEntry
                        let timecardEntryMessages = buildTimecardEntryMessages (get #id timecardEntry) selectedMessageIds
                        mapM_ createRecord timecardEntryMessages
                    redirectTo PersonSelectionAction {selectedPersonId}
    --
    action UpdateTimecardEntryAction = do
        ensureIsUser
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        let selectedMessageIds = param @[Id TwilioMessage] "selectedMessageIds"
        let timecardEntryId = param @(Id TimecardEntry) "timecardEntryId"
        timecardEntry <- fetch timecardEntryId
        timecardEntry
            |> buildTimecardEntry
            |> set #personId selectedPersonId
            |> ifValid \case
                Left timecardEntry -> do
                    botId <- fetchBotId
                    people <- fetchPeopleExcluding botId
                    messages <- fetchMessagesBetween botId selectedPersonId
                    toPhoneNumberId <- get #phoneNumberId <$> query @PhoneContact |> filterWhere (#personId, selectedPersonId) |> fetchOne
                    toPhoneNumber <- fetch toPhoneNumberId
                    selectedPerson <- fetch selectedPersonId
                    timecardEntry <- fetch timecardEntryId
                    let selectedMessages = findSelectedMessages messages selectedMessageIds
                    let timecardActivity = EditingEntry
                    let newMessage = newRecord @TwilioMessage
                    let personActivity = WorkingOnTimecardEntry {timecardEntry, selectedMessages, timecardActivity}
                    let personSelection = PersonSelected {selectedPerson, messages, toPhoneNumber, newMessage, personActivity}
                    render IndexView {people, personSelection}
                Right timecardEntry -> do
                    let timecardEntryMessages = buildTimecardEntryMessages timecardEntryId selectedMessageIds
                    withTransaction do
                        oldTimecardEntryMessages <- query @TimecardEntryMessage |> filterWhere (#timecardEntryId, timecardEntryId) |> fetch
                        deleteRecords oldTimecardEntryMessages
                        updateRecord timecardEntry
                        mapM_ createRecord timecardEntryMessages
                    redirectTo PersonSelectionAction {selectedPersonId}
    --
    action CreateOutgoingPhoneMessageAction = do
        ensureIsUser
        let toPhoneNumberId = Id (param "toId")
        let body = strip $ param "body"
        botId <- fetchBotId
        toPerson <- fetchPersonFor toPhoneNumberId
        fromPhoneNumber <- fetchPhoneNumberFor botId
        toPhoneNumber <- fetchOne toPhoneNumberId
        if body == ""
            then redirectTo $ PersonSelectionAction (get #id toPerson)
            else pure ()
        Twilio.Response {apiVersion, messageSid, accountSid, status, body, numMedia} <-
            Twilio.sendPhoneMessage
                twilioAccountId
                twilioAuthToken
                twilioStatusCallbackUrl
                (get #number fromPhoneNumber)
                (get #number toPhoneNumber)
                body
        newRecord @TwilioMessage
            |> set #apiVersion apiVersion
            |> set #messageSid messageSid
            |> set #accountSid accountSid
            |> set #fromId (get #id fromPhoneNumber)
            |> set #toId toPhoneNumberId
            |> set #status status
            |> set #body body
            |> set #numMedia numMedia
            |> createRecord
        redirectTo $ PersonSelectionAction (get #id toPerson)
    --
    action UpdateOutgoingPhoneMessageAction = do
        validateCallbackSignature
        let messageSid = param @Text "MessageSid"
        let messageStatus = param @Text "MessageStatus"
        twilioMessage <-
            query @TwilioMessage
                |> filterWhere (#messageSid, messageSid)
                |> fetchOne
        if get #status twilioMessage /= "delivered"
            then do
                twilioMessage
                    |> set #status messageStatus
                    |> updateRecord
                pure ()
            else pure ()
        renderPlain ""
    --
    action CreateIncomingPhoneMessageAction = do
        validateCallbackSignature
        let twilioMessage = buildIncomingTwilioMessage newRecord
        let fromNumber = param "From"
        let toNumber = param "To"
        fromPhoneNumber <-
            query @PhoneNumber
                |> filterWhere (#number, fromNumber)
                |> fetchOne
        toPhoneNumber <-
            query @PhoneNumber
                |> filterWhere (#number, toNumber)
                |> fetchOne
        twilioMessage
            |> set #fromId (get #id fromPhoneNumber)
            |> set #toId (get #id toPhoneNumber)
            |> createRecord
        renderPlain ""

fetchPeopleExcluding :: (?modelContext :: ModelContext) => Id Person -> IO [Person]
fetchPeopleExcluding idToExclude = do
    people <- query @Person |> orderByAsc #lastName |> fetch
    filter (\person -> get #id person /= idToExclude) people |> pure

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

fetchMessagesBetween :: (?modelContext :: ModelContext) => Id Person -> Id Person -> IO [Message]
fetchMessagesBetween personIdA personIdB = do
    trackTableRead "twilio_messages"
    sqlQuery messagesQuery (personIdA, personIdB)

findSelectedMessages :: [Message] -> [Id TwilioMessage] -> [Message]
findSelectedMessages messages selectedMessageIds =
    catMaybes $ findMessage <$> selectedMessageIds
  where
    findMessage messageId = find (\message -> get #id message == messageId) messages

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
        |> set #status (param "SmsStatus")
        |> set #body (param "Body")
        |> set #numMedia (param "NumMedia")

buildNewTimecardEntry :: UTCTime -> TimecardEntry -> TimecardEntry
buildNewTimecardEntry date timecardEntry =
    timecardEntry
        |> set #date date
        |> set #hoursWorked 8.0

buildTimecardEntry ::
    (?context :: ControllerContext) =>
    TimecardEntry ->
    TimecardEntry
buildTimecardEntry timecardEntry = do
    timecardEntry
        |> fill @["date", "jobName", "hoursWorked", "workDone", "invoiceTranslation"]
        |> validateField #jobName nonEmpty
        |> validateField #hoursWorked (validateAny [isInList [0.0], isGreaterThan 0.0])
        |> validateField #workDone nonEmpty
        |> validateField #invoiceTranslation nonEmpty

buildTimecardEntryMessages :: Id TimecardEntry -> [Id TwilioMessage] -> [TimecardEntryMessage]
buildTimecardEntryMessages timecardEntryId =
    map $ \messageId ->
        newRecord @TimecardEntryMessage
            |> set #timecardEntryId timecardEntryId
            |> set #twilioMessageId messageId

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

instance FromRow Message where
    fromRow =
        Message
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

instance FromField MessageStatus where
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
