{-# LANGUAGE PackageImports #-}

module Web.Controller.Communications where

import qualified Application.Service.Twilio as Twilio
import Data.Text (strip)
import Database.PostgreSQL.Simple (Query)
import Text.RawString.QQ (r)
import Web.Controller.Prelude
import Web.View.Communications.Index

instance Controller CommunicationsController where
    beforeAction = ensureIsUser

    action CommunicationsAction = autoRefresh do
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        let personSelection = NoPersonSelected

        render IndexView {..}
    --
    action PersonSelectionAction {..} = autoRefresh do
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId

        selectedPerson <- fetch selectedPersonId
        messages <- fetchMessagesBetween botId selectedPersonId
        toPhoneNumber <- fetchPhoneNumberFor selectedPersonId
        let newMessage = newRecord @TwilioMessage

        timecardEntries <- fetchTimecardEntriesFor selectedPersonId
        let personActivity = SendingMessage {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action NewTimecardEntryAction {..} = autoRefresh do
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        selectedPerson <- fetch selectedPersonId

        messages <- fetchMessagesBetween botId selectedPersonId
        toPhoneNumber <- fetchPhoneNumberFor selectedPersonId
        let selectedMessageIds = paramOrDefault @[Id TwilioMessage] [] "selectedMessageIds"
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        let newMessage = newRecord @TwilioMessage

        now <- getCurrentTime
        let timecardDate = maybe now (get #createdAt) (head selectedMessages)
        let timecardEntry = newRecord @TimecardEntry |> buildNewTimecardEntry timecardDate
        let timecardActivity = CreatingEntry

        let personActivity = WorkingOnTimecardEntry {..}
        let personSelection = PersonSelected {..}

        if null selectedMessageIds
            then redirectTo PersonSelectionAction {..}
            else render IndexView {..}
    --
    action EditTimecardEntryAction {..} = autoRefresh do
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        selectedPerson <- fetch selectedPersonId

        toPhoneNumber <- fetchPhoneNumberFor selectedPersonId
        messages <- fetchMessagesBetween botId selectedPersonId
        timecardEntryMessages <- fetchTimecardEntryMessagesFor timecardEntryId
        let selectedMessageIds = map (get #twilioMessageId) timecardEntryMessages
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        let newMessage = newRecord @TwilioMessage

        timecardEntry <- fetch timecardEntryId
        let timecardActivity = EditingEntry

        let personActivity = WorkingOnTimecardEntry {..}
        let personSelection = PersonSelected {..}

        if null selectedMessageIds
            then redirectTo PersonSelectionAction {..}
            else render IndexView {..}
    --
    action EditModifiedTimecardEntryAction {..} = autoRefresh do
        botId <- fetchBotId
        people <- fetchPeopleExcluding botId
        selectedPerson <- fetch selectedPersonId

        messages <- fetchMessagesBetween botId selectedPersonId
        toPhoneNumber <- fetchPhoneNumberFor selectedPersonId
        let selectedMessageIds = paramOrDefault @[Id TwilioMessage] [] "selectedMessageIds"
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        let newMessage = newRecord @TwilioMessage

        timecardEntry <- fetch timecardEntryId
        let timecardActivity = EditingModifiedEntry

        let personActivity = WorkingOnTimecardEntry {..}
        let personSelection = PersonSelected {..}

        if null selectedMessageIds
            then redirectTo PersonSelectionAction {..}
            else render IndexView {..}
    --
    action CreateTimecardEntryAction = do
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        let selectedMessageIds = param @[Id TwilioMessage] "selectedMessageIds"

        newRecord @TimecardEntry
            |> buildTimecardEntry
            |> set #personId selectedPersonId
            |> ifValid \case
                Left timecardEntry -> do
                    botId <- fetchBotId
                    people <- fetchPeopleExcluding botId
                    selectedPerson <- fetch selectedPersonId

                    messages <- fetchMessagesBetween botId selectedPersonId
                    toPhoneNumber <- fetchPhoneNumberFor selectedPersonId
                    let selectedMessages = findSelectedMessages messages selectedMessageIds
                    let newMessage = newRecord @TwilioMessage

                    let timecardActivity = CreatingEntry
                    let personActivity = WorkingOnTimecardEntry {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {..}
                Right timecardEntry -> do
                    withTransaction do
                        timecardEntry <- createRecord timecardEntry
                        let timecardEntryId = get #id timecardEntry
                        let timecardEntryMessages =
                                buildTimecardEntryMessages timecardEntryId selectedMessageIds
                        mapM_ createRecord timecardEntryMessages

                    redirectTo PersonSelectionAction {..}
    --
    action UpdateTimecardEntryAction = do
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
                    selectedPerson <- fetch selectedPersonId

                    messages <- fetchMessagesBetween botId selectedPersonId
                    toPhoneNumber <- fetchPhoneNumberFor selectedPersonId
                    let selectedMessages = findSelectedMessages messages selectedMessageIds
                    let newMessage = newRecord @TwilioMessage

                    timecardEntry <- fetch timecardEntryId
                    let timecardActivity = EditingEntry

                    let personActivity = WorkingOnTimecardEntry {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {people, personSelection}
                Right timecardEntry -> do
                    let timecardEntryMessages =
                            buildTimecardEntryMessages timecardEntryId selectedMessageIds
                    withTransaction do
                        oldTimecardEntryMessages <- fetchTimecardEntryMessagesFor timecardEntryId
                        deleteRecords oldTimecardEntryMessages
                        updateRecord timecardEntry
                        mapM_ createRecord timecardEntryMessages

                    redirectTo PersonSelectionAction {selectedPersonId}
    --
    action CreateOutgoingPhoneMessageAction = do
        let toPhoneNumberId = Id (param "toId")
        toPhoneNumber <- fetchOne toPhoneNumberId

        botId <- fetchBotId
        fromPhoneNumber <- fetchPhoneNumberFor botId
        toPerson <- fetchPersonFor toPhoneNumberId

        let body = strip $ param "body"
        if body == ""
            then redirectTo $ PersonSelectionAction (get #id toPerson)
            else pure ()

        Twilio.Response {..} <-
            Twilio.sendPhoneMessage
                Twilio.accountId
                Twilio.authToken
                Twilio.statusCallbackUrl
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

fetchPeopleExcluding ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO [Person]
fetchPeopleExcluding idToExclude = do
    people <- query @Person |> orderByAsc #lastName |> fetch
    filter (\person -> get #id person /= idToExclude) people |> pure

fetchPersonFor ::
    (?modelContext :: ModelContext) =>
    Id PhoneNumber ->
    IO Person
fetchPersonFor phoneNumberId = do
    phoneContact <-
        query @PhoneContact
            |> filterWhere (#phoneNumberId, phoneNumberId)
            |> fetchOne
    fetchOne (get #personId phoneContact)

fetchBotId ::
    (?modelContext :: ModelContext) =>
    IO (Id Person)
fetchBotId = get #id <$> fetchBot

fetchBot ::
    (?modelContext :: ModelContext) =>
    IO Person
fetchBot = query @Person |> filterWhere (#goesBy, botName) |> fetchOne

fetchPhoneNumberFor ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO PhoneNumber
fetchPhoneNumberFor personId = do
    phoneContact <-
        query @PhoneContact
            |> filterWhere (#personId, personId)
            |> fetchOne
    fetchOne (get #phoneNumberId phoneContact)

fetchMessagesBetween ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    Id Person ->
    IO [Message]
fetchMessagesBetween personIdA personIdB = do
    trackTableRead "twilio_messages"
    sqlQuery messagesQuery (personIdA, personIdB)

fetchTimecardEntryMessagesFor ::
    (?modelContext :: ModelContext) =>
    Id TimecardEntry ->
    IO [TimecardEntryMessage]
fetchTimecardEntryMessagesFor timecardEntryId = do
    query @TimecardEntryMessage
        |> filterWhere (#timecardEntryId, timecardEntryId)
        |> fetch

fetchTimecardEntriesFor ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO [TimecardEntry]
fetchTimecardEntriesFor personId =
    query @TimecardEntry
        |> filterWhere (#personId, personId)
        |> orderByDesc #date
        |> fetch

findSelectedMessages ::
    [Message] ->
    [Id TwilioMessage] ->
    [Message]
findSelectedMessages messages selectedMessageIds =
    catMaybes $ findMessage <$> selectedMessageIds
  where
    findMessage messageId = find (\message -> get #id message == messageId) messages

buildNewTimecardEntry ::
    UTCTime ->
    TimecardEntry ->
    TimecardEntry
buildNewTimecardEntry date timecardEntry =
    timecardEntry
        |> set #date date
        |> set #hoursWorked defaultHoursWorked

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

buildTimecardEntryMessages ::
    Id TimecardEntry ->
    [Id TwilioMessage] ->
    [TimecardEntryMessage]
buildTimecardEntryMessages timecardEntryId =
    map $ \messageId ->
        newRecord @TimecardEntryMessage
            |> set #timecardEntryId timecardEntryId
            |> set #twilioMessageId messageId

botName :: Text
botName = "Tim the Bot"

defaultHoursWorked :: Double
defaultHoursWorked = 8.0

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
