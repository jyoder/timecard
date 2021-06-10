{-# LANGUAGE PackageImports #-}

module Web.Controller.Communications where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Base.People as People
import qualified Application.Base.PhoneNumber as PhoneNumber
import Application.Service.Validation (validateAndCreate)
import qualified Application.Timecard.TimecardEntry as TimecardEntry
import qualified Application.Timecard.TimecardEntryMessage as TimecardEntryMessage
import qualified Application.Twilio.TwilioMessage as TwilioMessage
import Data.Text (strip)
import Data.Time.Calendar.WeekDate (toWeekDate)
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
        scheduledMessages <- SendMessageAction.fetchFutureFor selectedPersonId
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
        scheduledMessages <- SendMessageAction.fetchFutureFor selectedPersonId
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
        botId <- People.fetchBotId
        people <- People.fetchExcluding botId
        selectedPerson <- fetch selectedPersonId

        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
        timecardEntryMessages <- TimecardEntryMessage.fetchByTimecardEntry timecardEntryId
        let selectedMessageIds = map (get #twilioMessageId) timecardEntryMessages
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        scheduledMessages <- SendMessageAction.fetchFutureFor selectedPersonId
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
        scheduledMessages <- SendMessageAction.fetchFutureFor selectedPersonId
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
        selectedPerson <- fetch selectedPersonId

        newRecord @TimecardEntry
            |> buildTimecardEntry
            |> set #personId selectedPersonId
            |> ifValid \case
                Left timecardEntry -> do
                    botId <- People.fetchBotId
                    people <- People.fetchExcluding botId

                    messages <- TwilioMessage.fetchByPeople botId selectedPersonId
                    toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
                    let selectedMessages = findSelectedMessages messages selectedMessageIds
                    scheduledMessages <- SendMessageAction.fetchFutureFor selectedPersonId
                    let newMessage = newRecord @TwilioMessage

                    let timecardActivity = CreatingEntry
                    let personActivity = WorkingOnTimecardEntry {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {..}
                Right timecardEntry -> do
                    withTransaction do
                        scheduleNextTimecardEntryRequest timecardEntry selectedPerson
                        timecardEntry <- createRecord timecardEntry
                        let timecardEntryId = get #id timecardEntry
                        let timecardEntryMessages =
                                TimecardEntryMessage.buildAll timecardEntryId selectedMessageIds
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
                    botId <- People.fetchBotId
                    people <- People.fetchExcluding botId
                    selectedPerson <- fetch selectedPersonId

                    messages <- TwilioMessage.fetchByPeople botId selectedPersonId
                    toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
                    let selectedMessages = findSelectedMessages messages selectedMessageIds
                    scheduledMessages <- SendMessageAction.fetchFutureFor selectedPersonId
                    let newMessage = newRecord @TwilioMessage

                    timecardEntry <- fetch timecardEntryId
                    let timecardActivity = EditingEntry

                    let personActivity = WorkingOnTimecardEntry {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {people, personSelection}
                Right timecardEntry -> do
                    let timecardEntryMessages =
                            TimecardEntryMessage.buildAll timecardEntryId selectedMessageIds
                    withTransaction do
                        oldTimecardEntryMessages <- TimecardEntryMessage.fetchByTimecardEntry timecardEntryId
                        deleteRecords oldTimecardEntryMessages
                        updateRecord timecardEntry
                        mapM_ createRecord timecardEntryMessages

                    redirectTo PersonSelectionAction {selectedPersonId}
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

scheduleNextTimecardEntryRequest ::
    (?modelContext :: ModelContext) =>
    TimecardEntry ->
    Person ->
    IO ()
scheduleNextTimecardEntryRequest lastTimecardEntry person = do
    now <- getCurrentTime
    isScheduled <- alreadyScheduled
    if not isScheduled
        then do
            fromId <- fromPhoneNumberId
            toId <- toPhoneNumberId
            let sendAt = sendTime companyTimeZone now
            SendMessageAction.schedule fromId toId messageBody sendAt
            pure ()
        else pure ()
  where
    alreadyScheduled = do
        sendMessageActions <- SendMessageAction.fetchFutureFor (get #id person)
        pure $ not $ null sendMessageActions
    fromPhoneNumberId = do
        botId <- People.fetchBotId
        phoneNumber <- PhoneNumber.fetchByPerson botId
        pure $ get #id phoneNumber
    toPhoneNumberId = do
        phoneNumber <- PhoneNumber.fetchByPerson (get #id person)
        pure $ get #id phoneNumber
    messageBody = "Hey " <> get #goesBy person <> " - I've got you at " <> get #jobName lastTimecardEntry <> " today. Let me know what hours you worked and what you did when you have a chance. Thanks!"
    sendTime timeZone time =
        let localTime = utcToLocalTime timeZone time
         in localTimeToUTC
                timeZone
                if localTime < sendTimeToday localTime
                    then sendTimeToday localTime
                    else sendTimeNextWorkingDay localTime
    sendTimeToday now = LocalTime (localDay now) sendTimeOfDay
    sendTimeNextWorkingDay now = LocalTime (nextWorkingDay $ localDay now) sendTimeOfDay
    sendTimeOfDay = TimeOfDay 15 30 0
    nextWorkingDay today = case toWeekDate today of
        (_, _, 5) -> addDays 3 today
        (_, _, 6) -> addDays 2 today
        _ -> addDays 1 today

companyTimeZone :: TimeZone
companyTimeZone = read "PDT"

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
        |> TimecardEntry.validate

defaultHoursWorked :: Double
defaultHoursWorked = 8.0
