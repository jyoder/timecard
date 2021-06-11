{-# LANGUAGE PackageImports #-}

module Web.Controller.Communications where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Base.People as People
import qualified Application.Base.PhoneNumber as PhoneNumber
import qualified Application.Timecard.TimecardEntry as TimecardEntry
import qualified Application.Timecard.TimecardEntryMessage as TimecardEntryMessage
import qualified Application.Timecard.TimecardEntryRequest as TimecardEntryRequest
import qualified Application.Twilio.TwilioMessage as TwilioMessage
import Data.Text (strip)
import Text.Read (read)
import Web.Controller.Prelude
import Web.View.Communications.Index

instance Controller CommunicationsController where
    beforeAction = ensureIsUser

    action CommunicationsAction = autoRefresh do
        botId <- People.fetchBotId
        people <- People.fetchExcluding botId
        let personSelection = NoPersonSelected

        render IndexView {..}
    --
    action PersonSelectionAction {..} = autoRefresh do
        botId <- People.fetchBotId
        people <- People.fetchExcluding botId
        selectedPerson <- fetch selectedPersonId

        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        scheduledMessages <- SendMessageAction.fetchFutureByPhoneNumber (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        timecardEntries <- TimecardEntry.fetchByPerson selectedPersonId

        let personActivity = SendingMessage {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action NewTimecardEntryAction {..} = autoRefresh do
        botId <- People.fetchBotId
        people <- People.fetchExcluding botId
        selectedPerson <- fetch selectedPersonId

        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        let selectedMessageIds = paramOrDefault @[Id TwilioMessage] [] "selectedMessageIds"
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        scheduledMessages <- SendMessageAction.fetchFutureByPhoneNumber (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        now <- getCurrentTime
        let timecardDate = toLocalDay $ maybe now (get #createdAt) (head selectedMessages)
        let timecardEntry = newRecord @TimecardEntry |> buildNewTimecardEntry timecardDate
        let timecardActivity = CreatingEntry

        let personActivity = WorkingOnTimecardEntry {..}
        let personSelection = PersonSelected {..}

        if null selectedMessageIds
            then redirectTo PersonSelectionAction {..}
            else render IndexView {..}
      where
        toLocalDay = localDay . utcToLocalTime companyTimeZone
    --
    action EditTimecardEntryAction {..} = autoRefresh do
        botId <- People.fetchBotId
        people <- People.fetchExcluding botId
        selectedPerson <- fetch selectedPersonId

        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
        timecardEntryMessages <- TimecardEntryMessage.fetchByTimecardEntry timecardEntryId
        let selectedMessageIds = map (get #twilioMessageId) timecardEntryMessages
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        scheduledMessages <- SendMessageAction.fetchFutureByPhoneNumber (get #id toPhoneNumber)
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
        botId <- People.fetchBotId
        people <- People.fetchExcluding botId
        selectedPerson <- fetch selectedPersonId

        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        let selectedMessageIds = paramOrDefault @[Id TwilioMessage] [] "selectedMessageIds"
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        scheduledMessages <- SendMessageAction.fetchFutureByPhoneNumber (get #id toPhoneNumber)
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
        botId <- People.fetchBotId
        selectedPerson <- fetch selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId

        newRecord @TimecardEntry
            |> buildTimecardEntry
            |> set #personId selectedPersonId
            |> ifValid \case
                Left timecardEntry -> do
                    people <- People.fetchExcluding botId
                    messages <- TwilioMessage.fetchByPeople botId selectedPersonId
                    let selectedMessages = findSelectedMessages messages selectedMessageIds
                    scheduledMessages <-
                        SendMessageAction.fetchFutureByPhoneNumber
                            (get #id toPhoneNumber)
                    let newMessage = newRecord @TwilioMessage

                    let timecardActivity = CreatingEntry
                    let personActivity = WorkingOnTimecardEntry {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {..}
                Right timecardEntry -> do
                    fromPhoneNumber <- PhoneNumber.fetchByPerson botId
                    withTransaction do
                        timecardEntry <- createRecord timecardEntry
                        TimecardEntryMessage.replaceAllForTimecard
                            (get #id timecardEntry)
                            selectedMessageIds
                        scheduleNextRequest
                            timecardEntry
                            selectedPerson
                            (get #id fromPhoneNumber)
                            (get #id toPhoneNumber)
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
                    botId <- People.fetchBotId
                    people <- People.fetchExcluding botId
                    selectedPerson <- fetch selectedPersonId

                    messages <- TwilioMessage.fetchByPeople botId selectedPersonId
                    toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
                    let selectedMessages = findSelectedMessages messages selectedMessageIds
                    scheduledMessages <-
                        SendMessageAction.fetchFutureByPhoneNumber
                            (get #id toPhoneNumber)
                    let newMessage = newRecord @TwilioMessage

                    timecardEntry <- fetch timecardEntryId
                    let timecardActivity = EditingEntry

                    let personActivity = WorkingOnTimecardEntry {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {people, personSelection}
                Right timecardEntry -> do
                    withTransaction do
                        updateRecord timecardEntry
                        TimecardEntryMessage.replaceAllForTimecard
                            (get #id timecardEntry)
                            selectedMessageIds
                    redirectTo PersonSelectionAction {..}
    --
    action CreateOutgoingPhoneMessageAction = do
        let toPhoneNumberId = Id (param "toId")
        toPhoneNumber <- fetchOne toPhoneNumberId

        botId <- People.fetchBotId
        fromPhoneNumber <- PhoneNumber.fetchByPerson botId
        toPerson <- People.fetchByPhoneNumber toPhoneNumberId

        let body = strip $ param "body"
        if body == ""
            then redirectTo $ PersonSelectionAction (get #id toPerson)
            else do
                TwilioMessage.send fromPhoneNumber toPhoneNumber body
                redirectTo $ PersonSelectionAction (get #id toPerson)
    --
    action CancelScheduledMessageAction {..} = do
        sendMessageAction <- fetch sendMessageActionId
        actionRunState <- fetch (get #actionRunStateId sendMessageAction)
        toPerson <- People.fetchByPhoneNumber (get #toId sendMessageAction)

        ActionRunState.updateCanceled actionRunState

        redirectTo $ PersonSelectionAction (get #id toPerson)

findSelectedMessages ::
    [TwilioMessage.T] ->
    [Id TwilioMessage] ->
    [TwilioMessage.T]
findSelectedMessages messages selectedMessageIds =
    catMaybes $ findMessage <$> selectedMessageIds
  where
    findMessage messageId = find (\message -> get #id message == messageId) messages

scheduleNextRequest ::
    (?modelContext :: ModelContext) =>
    TimecardEntry ->
    Person ->
    Id PhoneNumber ->
    Id PhoneNumber ->
    IO ()
scheduleNextRequest lastEntry person fromId toId = do
    now <- getCurrentTime
    alreadyScheduled <- TimecardEntryRequest.scheduledRequestExists toId

    let body = TimecardEntryRequest.requestBody person lastEntry
    let sendAt = TimecardEntryRequest.nextRequestTime companyTimeZone now

    if not alreadyScheduled
        then SendMessageAction.schedule fromId toId body sendAt >> pure ()
        else pure ()

buildNewTimecardEntry ::
    Day ->
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
        |> TimecardEntry.validate

defaultHoursWorked :: Double
defaultHoursWorked = 8.0

companyTimeZone :: TimeZone
companyTimeZone = read "PDT"
