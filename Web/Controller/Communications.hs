{-# LANGUAGE PackageImports #-}

module Web.Controller.Communications where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Base.People as People
import qualified Application.Base.PhoneNumber as PhoneNumber
import qualified Application.Timecard.AccessToken as Timecard.AccessToken
import qualified Application.Timecard.EntryRequest as Timecard.EntryRequest
import qualified Application.Timecard.Queries as Timecard.Queries
import qualified Application.Timecard.Timecard as Timecard
import qualified Application.Timecard.TimecardEntry as TimecardEntry
import qualified Application.Timecard.TimecardEntryMessage as TimecardEntryMessage
import qualified Application.Twilio.TwilioMessage as TwilioMessage
import Data.Text (strip)
import Text.Read (read)
import Web.Controller.Prelude
import Web.View.Communications.Index

instance Controller CommunicationsController where
    beforeAction = ensureIsUser

    action CommunicationsAction = autoRefresh do
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        let personSelection = NoPersonSelected

        render IndexView {..}
    --
    action PersonSelectionAction {..} = autoRefresh do
        now <- getCurrentTime
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        selectedPerson <- fetch selectedPersonId

        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        scheduledMessages <- SendMessageAction.fetchAfterByPhoneNumber now (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        timecards <-
            Timecard.Queries.fetchByPerson
                Timecard.Queries.EntriesDateDescending
                selectedPersonId

        let personActivity = SendingMessage {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action NewTimecardEntryAction {..} = autoRefresh do
        now <- getCurrentTime
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        selectedPerson <- fetch selectedPersonId

        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        let selectedMessageIds = paramOrDefault @[Id TwilioMessage] [] "selectedMessageIds"
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        scheduledMessages <- SendMessageAction.fetchAfterByPhoneNumber now (get #id toPhoneNumber)
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
        now <- getCurrentTime
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        selectedPerson <- fetch selectedPersonId

        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
        timecardEntryMessages <- TimecardEntryMessage.fetchByTimecardEntry timecardEntryId
        let selectedMessageIds = map (get #twilioMessageId) timecardEntryMessages
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        scheduledMessages <- SendMessageAction.fetchAfterByPhoneNumber now (get #id toPhoneNumber)
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
        now <- getCurrentTime
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        selectedPerson <- fetch selectedPersonId

        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        let selectedMessageIds = paramOrDefault @[Id TwilioMessage] [] "selectedMessageIds"
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        scheduledMessages <- SendMessageAction.fetchAfterByPhoneNumber now (get #id toPhoneNumber)
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

        now <- getCurrentTime
        botId <- People.fetchBotId
        selectedPerson <- fetch selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId

        newRecord @TimecardEntry
            |> buildTimecardEntry
            |> TimecardEntry.create selectedPersonId selectedMessageIds
                >>= either
                    ( \timecardEntry -> do
                        people <- People.fetchExcludingId botId
                        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
                        let selectedMessages = findSelectedMessages messages selectedMessageIds
                        scheduledMessages <-
                            SendMessageAction.fetchAfterByPhoneNumber now (get #id toPhoneNumber)
                        let newMessage = newRecord @TwilioMessage

                        let timecardActivity = CreatingEntry
                        let personActivity = WorkingOnTimecardEntry {..}
                        let personSelection = PersonSelected {..}

                        render IndexView {..}
                    )
                    ( \timecardEntry -> do
                        fromPhoneNumber <- PhoneNumber.fetchByPerson botId
                        Timecard.EntryRequest.scheduleNextRequest
                            companyTimeZone
                            now
                            timecardEntry
                            selectedPerson
                            (get #id fromPhoneNumber)
                            (get #id toPhoneNumber)
                        redirectTo PersonSelectionAction {..}
                    )
    --
    action UpdateTimecardEntryAction = do
        now <- getCurrentTime
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        let selectedMessageIds = param @[Id TwilioMessage] "selectedMessageIds"

        let timecardEntryId = param @(Id TimecardEntry) "timecardEntryId"
        timecardEntry <- fetch timecardEntryId

        timecardEntry
            |> buildTimecardEntry
            |> TimecardEntry.update selectedMessageIds
                >>= either
                    ( \timecardEntry -> do
                        botId <- People.fetchBotId
                        people <- People.fetchExcludingId botId
                        selectedPerson <- fetch selectedPersonId

                        messages <- TwilioMessage.fetchByPeople botId selectedPersonId
                        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
                        let selectedMessages = findSelectedMessages messages selectedMessageIds
                        scheduledMessages <-
                            SendMessageAction.fetchAfterByPhoneNumber
                                now
                                (get #id toPhoneNumber)
                        let newMessage = newRecord @TwilioMessage

                        let timecardActivity = EditingEntry
                        let personActivity = WorkingOnTimecardEntry {..}
                        let personSelection = PersonSelected {..}

                        render IndexView {people, personSelection}
                    )
                    ( \timecardEntry ->
                        redirectTo PersonSelectionAction {..}
                    )
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
    --
    action CreateTimecardReview = do
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        let timecardId = param @(Id Timecard) "timecardId"

        now <- getCurrentTime
        let expiresAt = Timecard.AccessToken.expirationFrom now
        Timecard.AccessToken.create expiresAt timecardId

        redirectTo $ PersonSelectionAction selectedPersonId

findSelectedMessages ::
    [TwilioMessage.T] ->
    [Id TwilioMessage] ->
    [TwilioMessage.T]
findSelectedMessages messages selectedMessageIds =
    catMaybes $ findMessage <$> selectedMessageIds
  where
    findMessage messageId = find (\message -> get #id message == messageId) messages

buildNewTimecardEntry ::
    Day ->
    TimecardEntry ->
    TimecardEntry
buildNewTimecardEntry date timecardEntry =
    timecardEntry
        |> set #date date
        |> set #hoursWorked defaultHoursWorked

buildTimecardEntry ::
    ( ?context :: ControllerContext
    ) =>
    TimecardEntry ->
    TimecardEntry
buildTimecardEntry timecardEntry =
    timecardEntry
        |> fill @["date", "jobName", "hoursWorked", "workDone", "invoiceTranslation"]

defaultHoursWorked :: Double
defaultHoursWorked = 8.0

companyTimeZone :: TimeZone
companyTimeZone = read "PDT"
