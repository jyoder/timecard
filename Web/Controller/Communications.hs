{-# LANGUAGE PackageImports #-}

module Web.Controller.Communications where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Audit.Entry as Audit.Entry
import qualified Application.Base.PhoneNumber as PhoneNumber
import qualified Application.People.Person as Person
import qualified Application.People.Query as People.Query
import qualified Application.People.View as People.View
import Application.Service.Time (roundHours)
import qualified Application.Timecard.AccessToken as Timecard.AccessToken
import qualified Application.Timecard.Entry as Timecard.Entry
import qualified Application.Timecard.EntryMessage as Timecard.EntryMessage
import qualified Application.Timecard.EntryRequest as Timecard.EntryRequest
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Timecard.ReviewRequest as ReviewRequest
import qualified Application.Timecard.Timecard as Timecard
import qualified Application.Timecard.View as Timecard.View
import qualified Application.Twilio.Query as Twilio.Query
import qualified Application.Twilio.TwilioMessage as TwilioMessage
import qualified Application.Twilio.View as Twilio.View
import Data.Functor ((<&>))
import Data.Text (strip)
import IHP.FrameworkConfig (baseUrl)
import Text.Read (read)
import Web.Controller.Prelude hiding (baseUrl)
import Web.View.Communications.Index

instance Controller CommunicationsController where
    beforeAction = ensureIsUser

    action CommunicationsAction = autoRefresh do
        let currentColumn = PeopleColumn
        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers

        personSelection <- case people of
            firstPerson : _ -> do
                botId <- Person.fetchBotId
                let selectedPersonId = get #id firstPerson
                selectedPerson <- fetch selectedPersonId

                messages <-
                    Twilio.Query.fetchByPeople botId selectedPersonId
                        <&> Twilio.View.buildMessages
                toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
                scheduledMessages <-
                    SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                        (get #id toPhoneNumber)
                timecards <-
                    Timecard.View.buildTimecards
                        <$> Timecard.Query.fetchByPerson
                            Timecard.Query.EntriesDateDescending
                            selectedPersonId

                let newMessage = newRecord @TwilioMessage
                let editingScheduledMessageId = Nothing
                let personActivity = SendingMessage {..}
                pure PersonSelected {..}
            [] -> pure NoPersonSelected

        render IndexView {..}
    --
    action CommunicationsPersonSelectionAction {..} = autoRefresh do
        let currentColumn = maybe PeopleColumn paramToColumn column
        botId <- Person.fetchBotId
        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers
        selectedPerson <- fetch selectedPersonId

        messages <-
            Twilio.Query.fetchByPeople botId selectedPersonId
                <&> Twilio.View.buildMessages
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

        let editingScheduledMessageId = Nothing
        let personActivity = SendingMessage {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action CommunicationsNewTimecardEntryAction {..} = do
        let currentColumn = TimecardsColumn
        let column = Just $ columnToParam currentColumn
        botId <- Person.fetchBotId
        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers
        selectedPerson <- fetch selectedPersonId

        messages <-
            Twilio.Query.fetchByPeople botId selectedPersonId
                <&> Twilio.View.buildMessages
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId

        let selectedMessageIds' = textToId <$> selectedMessageIds
        let selectedMessages = findSelectedMessages messages selectedMessageIds'
        scheduledMessages <-
            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        now <- getCurrentTime
        let timecardDate = toLocalDay $ maybe now (get #createdAt) (head selectedMessages)
        let timecardEntry = newRecord @TimecardEntry |> buildNewTimecardEntry timecardDate
        let timecardActivity = CreatingEntry

        let editingScheduledMessageId = Nothing
        let personActivity = WorkingOnTimecardEntry {..}
        let personSelection = PersonSelected {..}

        if null selectedMessageIds'
            then redirectTo CommunicationsPersonSelectionAction {..}
            else render IndexView {..}
      where
        toLocalDay = localDay . utcToLocalTime companyTimeZone
    --
    action CommunicationsEditTimecardEntryAction {..} = do
        let currentColumn = TimecardsColumn
        let column = Just $ columnToParam currentColumn
        botId <- Person.fetchBotId
        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers
        selectedPerson <- fetch selectedPersonId

        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        messages <-
            Twilio.Query.fetchByPeople botId selectedPersonId
                <&> Twilio.View.buildMessages
        timecardEntryMessages <- Timecard.EntryMessage.fetchByTimecardEntry timecardEntryId
        let selectedMessageIds = map (get #twilioMessageId) timecardEntryMessages
        let selectedMessages = findSelectedMessages messages selectedMessageIds
        scheduledMessages <-
            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        timecardEntry <- fetch timecardEntryId
        let timecardActivity = EditingEntry

        let editingScheduledMessageId = Nothing
        let personActivity = WorkingOnTimecardEntry {..}
        let personSelection = PersonSelected {..}

        if null selectedMessageIds
            then redirectTo CommunicationsPersonSelectionAction {..}
            else render IndexView {..}
    --
    action CommunicationsEditModifiedTimecardEntryAction {..} = do
        let currentColumn = TimecardsColumn
        let column = Just $ columnToParam currentColumn
        botId <- Person.fetchBotId
        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers
        selectedPerson <- fetch selectedPersonId

        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        messages <-
            Twilio.Query.fetchByPeople botId selectedPersonId
                <&> Twilio.View.buildMessages

        let selectedMessageIds' = textToId <$> selectedMessageIds
        let selectedMessages = findSelectedMessages messages selectedMessageIds'
        scheduledMessages <-
            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                (get #id toPhoneNumber)
        let newMessage = newRecord @TwilioMessage

        timecardEntry <- fetch timecardEntryId
        let timecardActivity = EditingModifiedEntry

        let editingScheduledMessageId = Nothing
        let personActivity = WorkingOnTimecardEntry {..}
        let personSelection = PersonSelected {..}

        if null selectedMessageIds'
            then redirectTo CommunicationsPersonSelectionAction {..}
            else render IndexView {..}
    --
    action CommunicationsCreateTimecardEntryAction = do
        let currentColumn = TimecardsColumn
        let column = Just $ columnToParam currentColumn
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        let selectedMessageIds = param @[Id TwilioMessage] "selectedMessageIds"

        botId <- Person.fetchBotId
        selectedPerson <- fetch selectedPersonId
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId

        newRecord @TimecardEntry
            |> buildTimecardEntry
            |> Timecard.Entry.create
                (Just $ get #id currentUser)
                selectedPersonId
                (get #id toPhoneNumber)
                selectedMessageIds
                >>= either
                    ( \timecardEntry -> do
                        people <-
                            People.View.buildPeople
                                <$> People.Query.fetchActiveWorkers
                        messages <-
                            Twilio.Query.fetchByPeople botId selectedPersonId
                                <&> Twilio.View.buildMessages
                        let selectedMessages = findSelectedMessages messages selectedMessageIds
                        scheduledMessages <-
                            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                                (get #id toPhoneNumber)
                        let newMessage = newRecord @TwilioMessage

                        let editingScheduledMessageId = Nothing
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
        let currentColumn = TimecardsColumn
        let column = Just $ columnToParam currentColumn
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        toPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId
        let selectedMessageIds = param @[Id TwilioMessage] "selectedMessageIds"
        timecardEntry <- fetch timecardEntryId

        timecardEntry
            |> buildTimecardEntry
            |> Timecard.Entry.update
                (Just $ get #id currentUser)
                (get #id toPhoneNumber)
                selectedMessageIds
                >>= either
                    ( \timecardEntry -> do
                        botId <- Person.fetchBotId
                        people <-
                            People.View.buildPeople
                                <$> People.Query.fetchActiveWorkers
                        selectedPerson <- fetch selectedPersonId

                        messages <-
                            Twilio.Query.fetchByPeople botId selectedPersonId
                                <&> Twilio.View.buildMessages
                        let selectedMessages = findSelectedMessages messages selectedMessageIds
                        scheduledMessages <-
                            SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
                                (get #id toPhoneNumber)
                        let newMessage = newRecord @TwilioMessage

                        let editingScheduledMessageId = Nothing
                        let timecardActivity = EditingEntry
                        let personActivity = WorkingOnTimecardEntry {..}
                        let personSelection = PersonSelected {..}

                        render IndexView {..}
                    )
                    ( \timecardEntry ->
                        redirectTo CommunicationsPersonSelectionAction {..}
                    )
    --
    action CommunicationsDeleteTimecardEntryAction {selectedPersonId, timecardEntryId} = do
        let currentColumn = TimecardsColumn
        let column = Just $ columnToParam currentColumn
        phoneNumber <- PhoneNumber.fetchByPerson selectedPersonId

        Timecard.Entry.delete
            (Just $ get #id currentUser)
            (get #id phoneNumber)
            timecardEntryId

        redirectTo CommunicationsPersonSelectionAction {..}
    --
    action CommunicationsSendPhoneMessageAction = do
        let toPhoneNumberId = Id (param "toId")
        toPhoneNumber <- fetchOne toPhoneNumberId

        botId <- Person.fetchBotId
        fromPhoneNumber <- PhoneNumber.fetchByPerson botId
        toPerson <- Person.fetchByPhoneNumber toPhoneNumberId
        let selectedPersonId = get #id toPerson
        let column = Just $ columnToParam MessagesColumn

        let body = strip $ param "body"
        if body == ""
            then redirectTo CommunicationsPersonSelectionAction {..}
            else do
                TwilioMessage.send
                    (Just $ get #id currentUser)
                    fromPhoneNumber
                    toPhoneNumber
                    body
                redirectTo CommunicationsPersonSelectionAction {..}
    --
    action CommunicationsEditScheduledMessageAction {..} = do
        let currentColumn = MessagesColumn
        sendMessageAction <- fetch sendMessageActionId
        actionRunState <- fetch (get #actionRunStateId sendMessageAction)
        toPerson <- Person.fetchByPhoneNumber (get #toId sendMessageAction)
        let selectedPersonId = get #id toPerson

        botId <- Person.fetchBotId
        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers
        selectedPerson <- fetch selectedPersonId

        messages <-
            Twilio.Query.fetchByPeople botId selectedPersonId
                <&> Twilio.View.buildMessages
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

        let editingScheduledMessageId = Just sendMessageActionId
        let personActivity = SendingMessage {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action CommunicationsUpdateScheduledMessageAction {..} = do
        sendMessageAction <- fetch sendMessageActionId
        actionRunState <- fetch (get #actionRunStateId sendMessageAction)
        toPerson <- Person.fetchByPhoneNumber (get #toId sendMessageAction)
        let selectedPersonId = get #id toPerson
        let column = Just $ columnToParam MessagesColumn

        case paramOrNothing @Text "body" of
            Just body ->
                sendMessageAction
                    |> set #body body
                    |> SendMessageAction.validate
                    |> ifValid \case
                        Left sendMessageAction ->
                            pure ()
                        Right sendMessageAction -> do
                            sendMessageAction |> updateRecord
                            Audit.Entry.createScheduledMessageEdited
                                (get #id currentUser)
                                sendMessageAction
                                (get #runsAt actionRunState)
                            pure ()
            Nothing ->
                pure ()

        let state = paramOrNothing @Text "state"
        if state == Just ActionRunState.canceled
            then do
                ActionRunState.updateCanceled actionRunState
                Audit.Entry.createScheduledMessageCanceled
                    (Just $ get #id currentUser)
                    sendMessageAction
                    (get #runsAt actionRunState)
                pure ()
            else
                if state == Just ActionRunState.notStarted
                    then do
                        ActionRunState.updateNotStarted actionRunState
                        Audit.Entry.createScheduledMessageResumed
                            (Just $ get #id currentUser)
                            sendMessageAction
                            (get #runsAt actionRunState)
                        pure ()
                    else pure ()

        redirectTo $ CommunicationsPersonSelectionAction {..}
    --
    action CommunicationsCreateTimecardReview = do
        let selectedPersonId = param @(Id Person) "selectedPersonId"
        let timecardId = param @(Id Timecard) "timecardId"
        let column = Just $ columnToParam MessagesColumn

        botId <- Person.fetchBotId
        botPhoneNumber <- PhoneNumber.fetchByPerson botId

        selectedPerson <- fetch selectedPersonId
        selectedPersonPhoneNumber <- PhoneNumber.fetchByPerson selectedPersonId

        now <- getCurrentTime
        ReviewRequest.scheduleRequest
            (Just $ get #id currentUser)
            (baseUrl $ getFrameworkConfig ?context)
            now
            timecardId
            selectedPerson
            (get #id botPhoneNumber)
            (get #id selectedPersonPhoneNumber)

        redirectTo CommunicationsPersonSelectionAction {..}

findSelectedMessages ::
    [Twilio.View.Message] ->
    [Id TwilioMessage] ->
    [Twilio.View.Message]
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
             , "lunchDuration"
             , "workDone"
             , "invoiceTranslation"
             ]
        |> Timecard.Entry.setClockedInAt (param "clockedInAt")
        |> Timecard.Entry.setClockedOutAt (param "clockedOutAt")
        |> set #hoursWorked (roundHours $ param "hoursWorked")

paramToColumn :: Text -> Column
paramToColumn "people" = PeopleColumn
paramToColumn "messages" = MessagesColumn
paramToColumn "timecards" = TimecardsColumn
paramToColumn _ = PeopleColumn

defaultLunchDuration :: Maybe Int
defaultLunchDuration = Just 30

defaultHoursWorked :: Double
defaultHoursWorked = 8.0

companyTimeZone :: TimeZone
companyTimeZone = read "PST"
