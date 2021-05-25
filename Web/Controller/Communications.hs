{-# LANGUAGE PackageImports #-}

module Web.Controller.Communications where

import qualified Application.Service.Twilio as Twilio
import qualified Data.ByteString.Char8 as BS8
import qualified Data.TMap as TMap
import Data.Text (strip)
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
    action CommunicationsAction = do
        ensureIsUser
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        let selectedPerson = Nothing
        let communications = []
        let selectedCommunications = []
        let timecardEntries = []
        let newMessage = Nothing
        let newTimecardEntry = Nothing
        let editingTimecard = False
        render IndexView {..}
    --
    action CommunicationsForAction {..} = autoRefresh do
        ensureIsUser
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        selectedPerson <- Just <$> fetch selectedPersonId
        toPhoneNumber <- fetchPhoneNumberFor selectedPersonId
        communications <- fetchCommunicationsBetween botId selectedPersonId
        let selectedCommunicationIds = paramOrDefault @[Id TwilioMessage] [] "selectedCommunicationIds"
        let selectedCommunications = catMaybes $ (\s -> find (\c -> get #messageId c == s) communications) <$> selectedCommunicationIds
        timecardEntries <- query @TimecardEntry |> filterWhere (#personId, selectedPersonId) |> orderByDesc #date |> fetch
        let editingTimecard = False
        let newMessage = newRecord @TwilioMessage |> set #toId (get #id toPhoneNumber) |> Just
        let newTimecardEntry = case listToMaybe $ reverse selectedCommunicationIds of
                Just messageId ->
                    case find (\c -> get #messageId c == messageId) communications of
                        Just c -> Just $ newRecord @TimecardEntry |> set #date (get #createdAt c) |> set #hoursWorked 8.0
                        Nothing -> Nothing
                Nothing -> Nothing
        render IndexView {..}
    --
    action CreateTimecardEntry = do
        ensureIsUser
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        selectedPerson <- Just <$> fetch selectedPersonId
        communications <- fetchCommunicationsBetween botId selectedPersonId
        let messageIds = param @[Text] "linkedMessageIds"
        let selectedMessageIds = param @[Id TwilioMessage] "linkedMessageIds"
        let selectedCommunications = catMaybes $ (\s -> find (\c -> get #messageId c == s) communications) <$> param @[Id TwilioMessage] "linkedMessageIds"
        let timecardEntries = []
        let newMessage = Just $ newRecord @TwilioMessage
        let editingTimecard = False
        case listToMaybe messageIds of
            Just messageId -> do
                newRecord @TimecardEntry
                    |> buildTimecardEntry
                    |> set #personId selectedPersonId
                    |> ifValid \case
                        Left newTimecardEntry -> do
                            render IndexView {newTimecardEntry = Just newTimecardEntry, ..}
                        Right newTimecardEntry -> do
                            setSuccessMessage "Created timecard entry"
                            nte <- createRecord newTimecardEntry
                            forEach
                                selectedMessageIds
                                ( \mid -> do
                                    newRecord @TimecardEntryMessage
                                        |> set #twilioMessageId mid
                                        |> set #timecardEntryId (get #id nte)
                                        |> createRecord
                                    pure ()
                                )
                            redirectTo $ CommunicationsForAction {selectedPersonId, selectedCommunicationIds = []}
            Nothing -> do
                setErrorMessage "No selected messages"
                redirectTo CommunicationsAction
    --
    action UpdateTimecardEntry = do
        ensureIsUser
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        selectedPerson <- Just <$> fetch selectedPersonId
        communications <- fetchCommunicationsBetween botId selectedPersonId
        let messageIds = param @[Text] "linkedMessageIds"
        let selectedMessageIds = param @[Id TwilioMessage] "linkedMessageIds"
        let selectedCommunications = catMaybes $ (\s -> find (\c -> get #messageId c == s) communications) <$> param @[Id TwilioMessage] "linkedMessageIds"
        let timecardEntries = []
        let newMessage = Just $ newRecord @TwilioMessage
        timecardEntry <- fetch (param @(Id TimecardEntry) "timecardEntryId")
        let editingTimecard = False
        case listToMaybe messageIds of
            Just messageId -> do
                timecardEntry
                    |> buildTimecardEntry
                    |> set #personId selectedPersonId
                    |> ifValid \case
                        Left newTimecardEntry -> do
                            render IndexView {newTimecardEntry = Just newTimecardEntry, ..}
                        Right newTimecardEntry -> do
                            setSuccessMessage "Updated timecard entry"
                            withTransaction do
                                nte <- updateRecord newTimecardEntry
                                timecardEntryMessages <- query @TimecardEntryMessage |> filterWhere (#timecardEntryId, get #id nte) |> fetch
                                timecardEntryMessages |> deleteRecords
                                forEach
                                    selectedMessageIds
                                    ( \mid -> do
                                        newRecord @TimecardEntryMessage
                                            |> set #twilioMessageId mid
                                            |> set #timecardEntryId (get #id nte)
                                            |> createRecord
                                        pure ()
                                    )
                            redirectTo $ CommunicationsForAction {selectedPersonId, selectedCommunicationIds = []}
            Nothing -> do
                setErrorMessage "No selected messages"
                redirectTo CommunicationsAction
    --
    action EditTimecardEntry {..} = do
        ensureIsUser
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        timecardEntry <- fetch selectedTimecardEntryId
        let newTimecardEntry = Just timecardEntry
        let selectedPersonId = get #personId timecardEntry
        selectedPerson <- Just <$> fetch selectedPersonId
        communications <- fetchCommunicationsBetween botId selectedPersonId
        timecardEntryMessages <- query @TimecardEntryMessage |> filterWhere (#timecardEntryId, get #id timecardEntry) |> fetch
        let selectedTwilioMessageIds = fromMaybe [] $ paramOrNothing @[Id TwilioMessage] "selectedCommunicationIds"
        selectedMessages <-
            if showExistingMessages == "True"
                then mapM (fetch . get #twilioMessageId) timecardEntryMessages
                else fetch selectedTwilioMessageIds
        let selectedCommunicationIds = map (get #id) selectedMessages
        let selectedCommunications = catMaybes $ (\s -> find (\c -> get #messageId c == s) communications) <$> selectedCommunicationIds
        let timecardEntries = []
        let newMessage = Just $ newRecord @TwilioMessage
        let editingTimecard = True
        render IndexView {..}
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
            then redirectTo $ CommunicationsForAction (get #id toPerson) []
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
        redirectTo $ CommunicationsForAction (get #id toPerson) []
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

fetchCommunicationsBetween :: (?modelContext :: ModelContext) => Id Person -> Id Person -> IO [Communication]
fetchCommunicationsBetween personIdA personIdB = do
    trackTableRead "twilio_messages"
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
        |> set #status (param "SmsStatus")
        |> set #body (param "Body")
        |> set #numMedia (param "NumMedia")

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

communicationsQuery :: Query
communicationsQuery =
    [r|
select
    twilio_messages.id,
    people_a.goes_by name_a,
    people_b.goes_by name_b,
    (twilio_messages.from_id = phone_numbers_a.id) is_from_person_a,
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

instance FromRow Communication where
    fromRow =
        Communication
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
