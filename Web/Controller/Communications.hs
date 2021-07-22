{-# LANGUAGE PackageImports #-}

module Web.Controller.Communications where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Base.People as People
import qualified Application.Base.PhoneNumber as PhoneNumber
import qualified Application.Timecard.AccessToken as Timecard.AccessToken
import qualified Application.Timecard.Entry as Timecard.Entry
import qualified Application.Timecard.EntryMessage as Timecard.EntryMessage
import qualified Application.Timecard.EntryRequest as Timecard.EntryRequest
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Timecard.Timecard as Timecard
import qualified Application.Timecard.View as Timecard.View
import qualified Application.Twilio.Query as Twilio.Query
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
    action CommunicationsPersonSelectionAction {..} = autoRefresh do
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        selectedPerson <- fetch selectedPersonId

        messages <- Twilio.Query.fetchByPeople botId selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        scheduledMessages <-
            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        timecards <-
            Timecard.View.buildTimecards
                <$> Timecard.Query.fetchByPerson
                    Timecard.Query.EntriesDateDescending
                    selectedPersonId

        let editingScheduledMessage = False
        let personActivity = SendingMessage {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action CommunicationsNewTimecardEntryAction {..} = autoRefresh do
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        selectedPerson <- fetch selectedPersonId

        messages <- Twilio.Query.fetchByPeople botId selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId

        -- TODO: this is a workaround to deal with a bug in AutoRoute:
        -- https://github.com/digitallyinduced/ihp/issues/971
        let selectedMessageIds' = fromMaybe [] (tail (textToId <$> selectedMessageIds))
        let selectedMessages = findSelectedMessages messages selectedMessageIds'
        scheduledMessages <-
            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        now <- getCurrentTime
        let timecardDate = toLocalDay $ maybe now (get #createdAt) (head selectedMessages)
        let timecardEntry = newRecord @TimecardEntry |> buildNewTimecardEntry timecardDate
        let timecardActivity = CreatingEntry

        let editingScheduledMessage = False
        let personActivity = WorkingOnTimecardEntry {..}
        let personSelection = PersonSelected {..}

        if null selectedMessageIds'
            then redirectTo CommunicationsPersonSelectionAction {..}
            else render IndexView {..}
      where
        toLocalDay = localDay . utcToLocalTime companyTimeZone
    --
    action CommunicationsEditTimecardEntryAction {..} = autoRefresh do
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        selectedPerson <- fetch selectedPersonId

        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        messages <- Twilio.Query.fetchByPeople botId selectedPersonId
        timecardEntryMessages <- Timecard.EntryMessage.fetchByTimecardEntry timecardEntryId
        let selectedMessageIds = map (get #twilioMessageId) timecardEntryMessages
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        scheduledMessages <-
            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        timecardEntry <- fetch timecardEntryId
        let timecardActivity = EditingEntry

        let editingScheduledMessage = False
        let personActivity = WorkingOnTimecardEntry {..}
        let personSelection = PersonSelected {..}

        if null selectedMessageIds
            then redirectTo CommunicationsPersonSelectionAction {..}
            else render IndexView {..}
    --
    action CommunicationsEditModifiedTimecardEntryAction {..} = autoRefresh do
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        selectedPerson <- fetch selectedPersonId

        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        messages <- Twilio.Query.fetchByPeople botId selectedPersonId

        -- TODO: this is a workaround to deal with a bug in AutoRoute:
        -- https://github.com/digitallyinduced/ihp/issues/971
        let selectedMessageIds' = fromMaybe [] (tail (textToId <$> selectedMessageIds))
        let selectedMessages = findSelectedMessages messages selectedMessageIds'
        scheduledMessages <-
            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        timecardEntry <- fetch timecardEntryId
        let timecardActivity = EditingModifiedEntry

        let editingScheduledMessage = False
        let personActivity = WorkingOnTimecardEntry {..}
        let personSelection = PersonSelected {..}

        if null selectedMessageIds'
            then redirectTo CommunicationsPersonSelectionAction {..}
            else render IndexView {..}
    --
    action CommunicationsCreateTimecardEntryAction = do
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        let selectedMessageIds = param @[Id TwilioMessage] "selectedMessageIds"

        botId <- People.fetchBotId
        selectedPerson <- fetch selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId

        newRecord @TimecardEntry
            |> buildTimecardEntry
            |> Timecard.Entry.create selectedPersonId selectedMessageIds
                >>= either
                    ( \timecardEntry -> do
                        people <- People.fetchExcludingId botId
                        messages <- Twilio.Query.fetchByPeople botId selectedPersonId
                        let selectedMessages = findSelectedMessages messages selectedMessageIds
                        scheduledMessages <-
                            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                                (get #id toPhoneNumber)
                        let newMessage = newRecord @TwilioMessage

                        let editingScheduledMessage = False
                        let timecardActivity = CreatingEntry
                        let personActivity = WorkingOnTimecardEntry {..}
                        let personSelection = PersonSelected {..}

                        render IndexView {..}
                    )
                    ( \timecardEntry -> do
                        now <- getCurrentTime
                        fromPhoneNumber <- PhoneNumber.fetchByPerson botId
                        Timecard.EntryRequest.scheduleNextRequest
                            companyTimeZone
                            now
                            timecardEntry
                            selectedPerson
                            (get #id fromPhoneNumber)
                            (get #id toPhoneNumber)
                        redirectTo CommunicationsPersonSelectionAction {..}
                    )
    --
    action CommunicationsUpdateTimecardEntryAction {timecardEntryId} = do
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        let selectedMessageIds = param @[Id TwilioMessage] "selectedMessageIds"
        timecardEntry <- fetch timecardEntryId

        timecardEntry
            |> buildTimecardEntry
            |> Timecard.Entry.update selectedMessageIds
                >>= either
                    ( \timecardEntry -> do
                        botId <- People.fetchBotId
                        people <- People.fetchExcludingId botId
                        selectedPerson <- fetch selectedPersonId

                        messages <- Twilio.Query.fetchByPeople botId selectedPersonId
                        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
                        let selectedMessages = findSelectedMessages messages selectedMessageIds
                        scheduledMessages <-
                            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                                (get #id toPhoneNumber)
                        let newMessage = newRecord @TwilioMessage

                        let editingScheduledMessage = False
                        let timecardActivity = EditingEntry
                        let personActivity = WorkingOnTimecardEntry {..}
                        let personSelection = PersonSelected {..}

                        render IndexView {..}
                    )
                    ( \timecardEntry ->
                        redirectTo CommunicationsPersonSelectionAction {..}
                    )
    --
    action CommunicationsSendPhoneMessageAction = do
        let toPhoneNumberId = Id (param "toId")
        toPhoneNumber <- fetchOne toPhoneNumberId

        botId <- People.fetchBotId
        fromPhoneNumber <- PhoneNumber.fetchByPerson botId
        toPerson <- People.fetchByPhoneNumber toPhoneNumberId

        let body = strip $ param "body"
        if body == ""
            then redirectTo $ CommunicationsPersonSelectionAction (get #id toPerson)
            else do
                TwilioMessage.send fromPhoneNumber toPhoneNumber body
                redirectTo $ CommunicationsPersonSelectionAction (get #id toPerson)
    --
    action CommunicationsEditScheduledMessageAction {..} = do
        sendMessageAction <- fetch sendMessageActionId
        actionRunState <- fetch (get #actionRunStateId sendMessageAction)
        toPerson <- People.fetchByPhoneNumber (get #toId sendMessageAction)
        let selectedPersonId = get #id toPerson

        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        selectedPerson <- fetch selectedPersonId

        messages <- Twilio.Query.fetchByPeople botId selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        scheduledMessages <-
            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        timecards <-
            Timecard.View.buildTimecards
                <$> Timecard.Query.fetchByPerson
                    Timecard.Query.EntriesDateDescending
                    selectedPersonId

        let editingScheduledMessage = True
        let personActivity = SendingMessage {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action CommunicationsUpdateScheduledMessageAction {..} = do
        sendMessageAction <- fetch sendMessageActionId
        actionRunState <- fetch (get #actionRunStateId sendMessageAction)
        toPerson <- People.fetchByPhoneNumber (get #toId sendMessageAction)

        case paramOrNothing @Text "body" of
            Just body ->
                sendMessageAction
                    |> set #body body
                    |> SendMessageAction.validate
                    |> ifValid \case
                        Left sendMessageAction ->
                            pure ()
                        Right sendMessageAction ->
                            sendMessageAction
                                |> updateRecord
                                >> pure ()
            Nothing ->
                pure ()

        let state = paramOrNothing @Text "state"
        if state == Just ActionRunState.canceled
            then ActionRunState.updateCanceled actionRunState >> pure ()
            else
                if state == Just ActionRunState.notStarted
                    then ActionRunState.updateNotStarted actionRunState >> pure ()
                    else pure ()

        redirectTo $ CommunicationsPersonSelectionAction (get #id toPerson)
    --
    action CommunicationsCreateTimecardReview = do
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        let timecardId = param @(Id Timecard) "timecardId"

        now <- getCurrentTime
        let expiresAt = Timecard.AccessToken.expirationFrom now
        Timecard.AccessToken.create expiresAt timecardId

        redirectTo $ CommunicationsPersonSelectionAction selectedPersonId

findSelectedMessages ::
    [Twilio.Query.Row] ->
    [Id TwilioMessage] ->
    [Twilio.Query.Row]
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
        |> set #lunchDuration defaultLunchDuration
        |> set #hoursWorked defaultHoursWorked

buildTimecardEntry ::
    ( ?context :: ControllerContext
    ) =>
    TimecardEntry ->
    TimecardEntry
buildTimecardEntry timecardEntry =
    timecardEntry
        |> fill
            @[ "date"
             , "jobName"
             , "clockedInAt"
             , "clockedOutAt"
             , "lunchDuration"
             , "hoursWorked"
             , "workDone"
             , "invoiceTranslation"
             ]

defaultLunchDuration :: Maybe Int
defaultLunchDuration = Just 30

defaultHoursWorked :: Double
defaultHoursWorked = 8.0

companyTimeZone :: TimeZone
companyTimeZone = read "PDT"
