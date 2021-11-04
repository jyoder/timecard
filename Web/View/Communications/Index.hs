module Web.View.Communications.Index where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.People.Person as Person
import qualified Application.People.View as V
import qualified Application.Timecard.View as V
import qualified Application.Twilio.Query as Twilio.Query
import qualified Application.Twilio.View as V
import qualified Text.Blaze.Html5 as H
import Web.View.Navigation.People
import Web.View.Navigation.Section (Section (Communications), renderSectionNavigation)
import Web.View.Prelude hiding (Page)
import Web.View.Service.Style (removeScrollbars)
import Web.View.Service.Time (formatDateTime, formatDay)
import Web.View.Timecards.Status

data IndexView = IndexView
    { people :: ![V.Person]
    , personSelection :: !PersonSelection
    , currentColumn :: !Column
    }

data PersonSelection
    = NoPersonSelected
    | PersonSelected
        { selectedPerson :: !Person
        , messages :: ![V.Message]
        , toPhoneNumber :: !PhoneNumber
        , scheduledMessages :: ![SendMessageAction.T]
        , editingScheduledMessageId :: !(Maybe (Id SendMessageAction))
        , newMessage :: !TwilioMessage
        , personActivity :: !PersonActivity
        }

data PersonActivity
    = SendingMessage
        { timecards :: ![V.Timecard]
        }
    | WorkingOnTimecardEntry
        { timecardEntry :: !TimecardEntry
        , selectedMessages :: ![V.Message]
        , timecardActivity :: TimecardActivity
        }

data TimecardActivity
    = CreatingEntry
    | EditingEntry
    | EditingModifiedEntry
    deriving (Eq)

data Page = Page
    { selectedPerson :: !(Maybe Person)
    , peopleNavigationClasses :: !Text
    , peopleNavigation :: !(PeopleNavigation CommunicationsController)
    , messagesColumnClasses :: !Text
    , messagesColumn :: !MessagesColumn
    , timecardsColumnClasses :: !Text
    , timecardsColumn :: !TimecardsColumn
    , columnNavigation :: !ColumnNavigation
    }
    deriving (Eq, Show)

data MessagesColumn
    = MessagesColumnNotVisible
    | MessagesColumnVisible
        { messageItems :: ![MessageItem]
        , scheduledMessageItems :: ![ScheduledMessageItem]
        , sendMessageForm :: !SendMessageForm
        }
    deriving (Eq, Show)

data MessageItem = MessageItem
    { fromName :: !Text
    , sentAt :: !Text
    , entities :: ![Entity]
    , statusClass :: !Text
    , status :: !Text
    , linkButtonActiveClass :: !Text
    , linkButtonText :: !Text
    , linkButtonAction :: !CommunicationsController
    }
    deriving (Eq, Show)

data Entity
    = RecognizedEntity
        { classes :: !Text
        , content :: !Text
        , tooltip :: !Text
        }
    | UnrecognizedEntity
        { content :: !Text
        }
    deriving (Eq, Show)

data ScheduledMessageItem
    = NotStartedScheduledMessageItem
        { runsAt :: !Text
        , body :: !Text
        , editAction :: !CommunicationsController
        , updateAction :: !CommunicationsController
        }
    | SuspendedScheduledMessageItem
        { runsAt :: !Text
        , body :: !Text
        , editAction :: !CommunicationsController
        , updateAction :: !CommunicationsController
        }
    | EditScheduledMessageForm
        { runsAt :: !Text
        , body :: !Text
        , sendMessageActionId :: !Text
        , saveAction :: !CommunicationsController
        , cancelAction :: !CommunicationsController
        }
    deriving (Eq, Show)

newtype SendMessageForm = SendMessageForm
    { toPhoneNumberId :: Text
    }
    deriving (Eq, Show)

data TimecardsColumn
    = TimecardList
        { timecardBlocks :: ![TimecardBlock]
        }
    | NewTimecardEntry
        { timecardEntryForm :: !TimecardEntryForm
        }
    | EditTimecardEntry
        { timecardEntryForm :: !TimecardEntryForm
        }
    | EditModifiedTimecardEntry
        { timecardEntryForm :: !TimecardEntryForm
        }
    deriving (Eq, Show)

data TimecardBlock = TimecardBlock
    { weekOf :: !Text
    , status :: !TimecardStatus
    , actions :: !TimecardActions
    , entryCards :: ![TimecardEntryCard]
    }
    deriving (Eq, Show)

data TimecardActions
    = TimecardInProgress
    | TimecardReadyForReview
        { selectedPersonId :: !Text
        , timecardId :: !Text
        }
    | TimecardUnderReview
        { reviewAction :: !TimecardReviewsController
        }
    | TimecardSigned
    deriving (Eq, Show)

data TimecardEntryCard = TimecardEntryCard
    { dayOfWeek' :: !Text
    , date :: !Text
    , jobName :: !Text
    , invoiceTranslation :: !Text
    , editAction :: !CommunicationsController
    , deleteAction :: !CommunicationsController
    }
    deriving (Eq, Show)

data TimecardEntryForm = TimecardEntryForm
    { date :: !Text
    , dateInvalidClass :: !Text
    , dateError :: !(Maybe Violation)
    , jobName :: !Text
    , jobNameInvalidClass :: !Text
    , jobNameError :: !(Maybe Violation)
    , clockedInAt :: !Text
    , clockedInAtInvalidClass :: !Text
    , clockedInAtError :: !(Maybe Violation)
    , clockedOutAt :: !Text
    , clockedOutAtInvalidClass :: !Text
    , clockedOutAtError :: !(Maybe Violation)
    , lunchDuration :: !Text
    , lunchDurationInvalidClass :: !Text
    , lunchDurationError :: !(Maybe Violation)
    , hoursWorked :: !Text
    , hoursWorkedInvalidClass :: !Text
    , hoursWorkedError :: !(Maybe Violation)
    , workDone :: !Text
    , workDoneInvalidClass :: !Text
    , workDoneError :: !(Maybe Violation)
    , invoiceTranslation :: !Text
    , invoiceTranslationInvalidClass :: !Text
    , invoiceTranslationError :: !(Maybe Violation)
    , selectedMessageIdsParam :: !Text
    , selectedPersonIdParam :: !Text
    , submitLabel :: !Text
    , submitAction :: !CommunicationsController
    , cancelAction :: !CommunicationsController
    }
    deriving (Eq, Show)

data ColumnNavigation = ColumnNavigation
    { peopleLinkClass :: !Text
    , peopleAction :: !CommunicationsController
    , messagesLinkClass :: !Text
    , messagesAction :: !CommunicationsController
    , timecardsLinkClass :: !Text
    , timecardsAction :: !CommunicationsController
    }
    deriving (Eq, Show)

data Column
    = PeopleColumn
    | MessagesColumn
    | TimecardsColumn
    deriving (Eq, Show)

instance View IndexView where
    html view = renderPage $ buildPage view

buildPage :: IndexView -> Page
buildPage view =
    let IndexView {..} = view
     in Page
            { selectedPerson = selectedPerson
            , peopleNavigationClasses = columnClasses PeopleColumn currentColumn
            , peopleNavigation =
                buildPeopleNavigation
                    BadgesVisible
                    ( \selectedPersonId ->
                        CommunicationsPersonSelectionAction
                            { selectedPersonId
                            , column = Just $ columnToParam MessagesColumn
                            }
                    )
                    selectedPerson
                    (get #people view)
            , messagesColumnClasses = columnClasses MessagesColumn currentColumn
            , messagesColumn = buildMessagesColumn view
            , timecardsColumnClasses = columnClasses TimecardsColumn currentColumn
            , timecardsColumn = buildTimecardsColumn view
            , columnNavigation = buildColumnNavigation personSelection currentColumn
            }
  where
    selectedPerson = case get #personSelection view of
        NoPersonSelected -> Nothing
        PersonSelected {..} -> Just selectedPerson

columnClasses :: Column -> Column -> Text
columnClasses column currentColumn =
    if currentColumn == column
        then "d-flex flex-grow-1 flex-lg-grow-0"
        else "d-none d-lg-flex"

buildMessagesColumn :: IndexView -> MessagesColumn
buildMessagesColumn IndexView {..} =
    case personSelection of
        NoPersonSelected -> MessagesColumnNotVisible
        PersonSelected {..} ->
            MessagesColumnVisible
                { messageItems = buildMessageItems selectedPerson personActivity messages
                , scheduledMessageItems =
                    buildScheduledMessageItem
                        editingScheduledMessageId
                        (get #id selectedPerson)
                        <$> scheduledMessages
                , sendMessageForm = buildSendMessageForm toPhoneNumber
                }

buildMessageItems :: Person -> PersonActivity -> [V.Message] -> [MessageItem]
buildMessageItems selectedPerson personActivity messages =
    let selectedMessageIds = case personActivity of
            SendingMessage {..} -> []
            WorkingOnTimecardEntry {..} -> get #id <$> selectedMessages
     in buildMessageItem selectedPerson personActivity selectedMessageIds <$> messages

buildMessageItem ::
    Person ->
    PersonActivity ->
    [Id TwilioMessage] ->
    V.Message ->
    MessageItem
buildMessageItem selectedPerson personActivity selectedMessageIds message =
    MessageItem
        { fromName = get #fromFirstName message <> " " <> get #fromLastName message
        , sentAt = formatDateTime $ get #createdAt message
        , entities = buildEntity <$> get #entities message
        , statusClass = messageStatusClass'
        , status = messageStatus
        , linkButtonActiveClass = if isSelected then "active" else ""
        , linkButtonText = if isSelected then "Unlink" else "Link"
        , linkButtonAction
        }
  where
    messageStatusClass' = messageStatusClass $ get #status message
    messageStatus = show $ get #status message
    linkButtonAction = case personActivity of
        SendingMessage {..} ->
            CommunicationsNewTimecardEntryAction
                { selectedMessageIds = toggledMessageIds
                , ..
                }
        WorkingOnTimecardEntry {..} -> case timecardActivity of
            CreatingEntry ->
                CommunicationsNewTimecardEntryAction
                    { selectedMessageIds = toggledMessageIds
                    , ..
                    }
            EditingEntry ->
                CommunicationsEditModifiedTimecardEntryAction
                    { selectedMessageIds = toggledMessageIds
                    , timecardEntryId = get #id timecardEntry
                    , ..
                    }
            EditingModifiedEntry ->
                CommunicationsEditModifiedTimecardEntryAction
                    { selectedMessageIds = toggledMessageIds
                    , timecardEntryId = get #id timecardEntry
                    , ..
                    }
    selectedPersonId = get #id selectedPerson
    toggledMessageIds = show <$> if isSelected then excludeMessageId else includeMessageId
    isSelected = messageId `elem` selectedMessageIds
    excludeMessageId = filter (/= messageId) selectedMessageIds
    includeMessageId = messageId : selectedMessageIds
    messageId = get #id message

buildEntity :: V.Entity -> Entity
buildEntity entity =
    let V.Entity {..} = entity
     in case entityType of
            Twilio.Query.Unrecognized ->
                UnrecognizedEntity
                    { content = rawText
                    }
            _ ->
                RecognizedEntity
                    { classes = entityClass entityType
                    , content = rawText
                    , tooltip = entityTooltip entity
                    }

entityClass :: Twilio.Query.EntityType -> Text
entityClass entityType =
    case entityType of
        Twilio.Query.JobName -> "entity job-name"
        Twilio.Query.HoursWorked -> "entity hours-worked"
        Twilio.Query.ClockedInAt -> "entity clocked-in-at"
        Twilio.Query.ClockedOutAt -> "entity clocked-out-at"
        Twilio.Query.TimeOnTask -> "entity time-on-task"
        Twilio.Query.WorkDone -> "entity work-done"
        Twilio.Query.Unrecognized -> "entity unrecognized"

entityTooltip :: V.Entity -> Text
entityTooltip V.Entity {..} =
    show entityType <> ": " <> show confidencePercent <> "%"
  where
    confidencePercent = round (confidence * 100)

messageStatusClass :: Twilio.Query.Status -> Text
messageStatusClass status =
    case status of
        Twilio.Query.Delivered -> "message-status delivered"
        Twilio.Query.Received -> "message-status received"
        Twilio.Query.Failed -> "message-status failed"
        _ -> "message-status sending"

buildScheduledMessageItem ::
    Maybe (Id SendMessageAction) ->
    Id Person ->
    SendMessageAction.T ->
    ScheduledMessageItem
buildScheduledMessageItem
    editingScheduledMessageId
    selectedPersonId
    scheduledMessage
        | editingScheduledMessageId == Just (get #id scheduledMessage) =
            buildEditScheduledMessageForm selectedPersonId scheduledMessage
        | get #state scheduledMessage == ActionRunState.suspended =
            buildSuspendedScheduledMessageItem scheduledMessage
        | otherwise = buildNotStartedScheduledMessageItem scheduledMessage

buildSuspendedScheduledMessageItem :: SendMessageAction.T -> ScheduledMessageItem
buildSuspendedScheduledMessageItem scheduledMessage =
    SuspendedScheduledMessageItem
        { runsAt = formatDateTime $ get #runsAt scheduledMessage
        , body = get #body scheduledMessage
        , editAction = editAction
        , updateAction = updateAction
        }
  where
    editAction = CommunicationsEditScheduledMessageAction (get #id scheduledMessage)
    updateAction = CommunicationsUpdateScheduledMessageAction (get #id scheduledMessage)

buildNotStartedScheduledMessageItem :: SendMessageAction.T -> ScheduledMessageItem
buildNotStartedScheduledMessageItem scheduledMessage =
    NotStartedScheduledMessageItem
        { runsAt = formatDateTime $ get #runsAt scheduledMessage
        , body = get #body scheduledMessage
        , editAction = editAction
        , updateAction = updateAction
        }
  where
    editAction = CommunicationsEditScheduledMessageAction (get #id scheduledMessage)
    updateAction = CommunicationsUpdateScheduledMessageAction (get #id scheduledMessage)

buildEditScheduledMessageForm :: Id Person -> SendMessageAction.T -> ScheduledMessageItem
buildEditScheduledMessageForm selectedPersonId scheduledMessage =
    EditScheduledMessageForm
        { runsAt = formatDateTime $ get #runsAt scheduledMessage
        , body = get #body scheduledMessage
        , sendMessageActionId = show $ get #id scheduledMessage
        , saveAction = saveAction
        , cancelAction = cancelAction
        }
  where
    saveAction =
        CommunicationsUpdateScheduledMessageAction
            { sendMessageActionId = get #id scheduledMessage
            }
    cancelAction =
        CommunicationsPersonSelectionAction
            { selectedPersonId
            , column = Just $ columnToParam MessagesColumn
            }

buildSendMessageForm :: PhoneNumber -> SendMessageForm
buildSendMessageForm toPhoneNumber =
    SendMessageForm
        { toPhoneNumberId = show $ get #id toPhoneNumber
        }

buildTimecardsColumn :: IndexView -> TimecardsColumn
buildTimecardsColumn IndexView {..} =
    case personSelection of
        NoPersonSelected -> TimecardList []
        PersonSelected {..} ->
            case personActivity of
                SendingMessage {..} ->
                    TimecardList $
                        buildTimecardBlock selectedPerson
                            <$> timecards
                WorkingOnTimecardEntry {..} ->
                    EditTimecardEntry $
                        buildTimecardEntryForm
                            selectedPerson
                            selectedMessages
                            timecardActivity
                            timecardEntry

buildTimecardBlock :: Person -> V.Timecard -> TimecardBlock
buildTimecardBlock selectedPerson timecard =
    TimecardBlock
        { weekOf = formatDay $ get #weekOf timecard
        , status = timecardStatus (get #status timecard)
        , actions = buildTimecardActions selectedPerson timecard
        , entryCards = buildTimecardEntryCard selectedPerson <$> get #entries timecard
        }

buildTimecardActions :: Person -> V.Timecard -> TimecardActions
buildTimecardActions selectedPerson timecard =
    case get #status timecard of
        V.TimecardInProgress -> TimecardInProgress
        V.TimecardReadyForReview ->
            TimecardReadyForReview
                { selectedPersonId = show $ get #id selectedPerson
                , timecardId = show $ get #id timecard
                }
        V.TimecardUnderReview V.AccessToken {..} ->
            TimecardUnderReview
                { reviewAction = ShowTimecardReviewAction value
                }
        V.TimecardSigned _ -> TimecardSigned

buildTimecardEntryCard :: Person -> V.TimecardEntry -> TimecardEntryCard
buildTimecardEntryCard selectedPerson timecardEntry =
    TimecardEntryCard
        { dayOfWeek' = show $ dayOfWeek $ get #date timecardEntry
        , date = formatDay $ get #date timecardEntry
        , jobName = get #jobName timecardEntry
        , invoiceTranslation = get #invoiceTranslation timecardEntry
        , editAction =
            CommunicationsEditTimecardEntryAction
                (get #id selectedPerson)
                (get #id timecardEntry)
        , deleteAction =
            CommunicationsDeleteTimecardEntryAction
                (get #id selectedPerson)
                (get #id timecardEntry)
        }

buildTimecardEntryForm ::
    Person ->
    [V.Message] ->
    TimecardActivity ->
    TimecardEntry ->
    TimecardEntryForm
buildTimecardEntryForm
    selectedPerson
    selectedMessages
    timecardActivity
    timecardEntry =
        TimecardEntryForm
            { date = show $ get #date timecardEntry
            , dateInvalidClass = if hasErrorFor "date" then invalidClass else ""
            , dateError = errorFor "date"
            , jobName = get #jobName timecardEntry
            , jobNameInvalidClass = if hasErrorFor "jobName" then invalidClass else ""
            , jobNameError = errorFor "jobName"
            , clockedInAt = maybe "" show (get #clockedInAt timecardEntry)
            , clockedInAtInvalidClass = if hasErrorFor "clockedInAt" then invalidClass else ""
            , clockedInAtError = errorFor "clockedInAt"
            , clockedOutAt = maybe "" show (get #clockedOutAt timecardEntry)
            , clockedOutAtInvalidClass = if hasErrorFor "clockedOutAt" then invalidClass else ""
            , clockedOutAtError = errorFor "clockedOutAt"
            , lunchDuration = maybe "" show (get #lunchDuration timecardEntry)
            , lunchDurationInvalidClass = if hasErrorFor "lunchDuration" then invalidClass else ""
            , lunchDurationError = errorFor "lunchDuration"
            , hoursWorked = show $ get #hoursWorked timecardEntry
            , hoursWorkedInvalidClass = if hasErrorFor "hoursWorked" then invalidClass else ""
            , hoursWorkedError = errorFor "hoursWorked"
            , workDone = workDone
            , workDoneInvalidClass = if hasErrorFor "workDone" then invalidClass else ""
            , workDoneError = errorFor "workDone"
            , invoiceTranslation = invoiceTranslations
            , invoiceTranslationInvalidClass = if hasErrorFor "invoiceTranslation" then invalidClass else ""
            , invoiceTranslationError = errorFor "invoiceTranslation"
            , selectedMessageIdsParam = selectedMessageIdsParam
            , selectedPersonIdParam = selectedPersonId
            , submitLabel = if timecardActivity == CreatingEntry then "Create" else "Update"
            , submitAction = if timecardActivity == CreatingEntry then createAction else updateAction
            , cancelAction
            }
      where
        hasErrorFor = isJust . errorFor
        errorFor fieldName = snd <$> find (\(name, _) -> name == fieldName) annotations
        annotations = timecardEntry |> get #meta |> get #annotations
        invalidClass = "is-invalid"
        workDone = assembleMessageBodies (get #workDone timecardEntry) sortedMessages
        invoiceTranslations = assembleMessageBodies (get #invoiceTranslation timecardEntry) sortedMessages
        selectedPersonId = show $ get #id selectedPerson
        selectedMessageIdsParam = intercalate "," (show . get #id <$> sortedMessages)
        sortedMessages = sortBy (\m1 m2 -> get #createdAt m1 `compare` get #createdAt m2) selectedMessages
        submitAction = if timecardActivity == CreatingEntry then createAction else updateAction
        createAction = CommunicationsCreateTimecardEntryAction
        updateAction =
            CommunicationsUpdateTimecardEntryAction
                { timecardEntryId = get #id timecardEntry
                }
        cancelAction =
            CommunicationsPersonSelectionAction
                { selectedPersonId = get #id selectedPerson
                , column = Just $ columnToParam TimecardsColumn
                }

assembleMessageBodies :: Text -> [V.Message] -> Text
assembleMessageBodies existingText messages =
    if existingText == ""
        then intercalate "\n\n" (get #body <$> messages)
        else existingText

buildColumnNavigation :: PersonSelection -> Column -> ColumnNavigation
buildColumnNavigation personSelection currentColumn =
    ColumnNavigation
        { peopleLinkClass = linkClass PeopleColumn
        , peopleAction = action PeopleColumn
        , messagesLinkClass = linkClass MessagesColumn
        , messagesAction = action MessagesColumn
        , timecardsLinkClass = linkClass TimecardsColumn
        , timecardsAction = action TimecardsColumn
        }
  where
    linkClass column = if column == currentColumn then "text-dark" else "text-muted"
    action column = case personSelection of
        NoPersonSelected -> CommunicationsAction
        PersonSelected {..} ->
            CommunicationsPersonSelectionAction
                { selectedPersonId = get #id selectedPerson
                , column = Just $ columnToParam column
                }

columnToParam :: Column -> Text
columnToParam PeopleColumn = "people"
columnToParam MessagesColumn = "messages"
columnToParam TimecardsColumn = "timecards"

renderPage :: Page -> Html
renderPage Page {..} =
    [hsx|
        <div class="d-flex flex-column">
            {renderSectionNavigation Communications selectedPerson}
            {renderColumnNavigation columnNavigation}
            
            <div class="d-flex flex-row">
                <div class={"mr-lg-3 flex-column " <> peopleNavigationClasses}>
                    {renderPeopleNavigation peopleNavigation}
                </div>
                <div class={"flex-lg-grow-1 flex-column-reverse flex-lg-column " <> messagesColumnClasses}>
                    {renderMessagesColumn messagesColumn}
                </div>
                <div class={"ml-lg-3 flex-column " <> timecardsColumnClasses}>
                    {renderTimecardsColumn timecardsColumn}
                </div>
            </div>
        </div>

        {styles}
    |]

renderMessagesColumn :: MessagesColumn -> Html
renderMessagesColumn messagesColumn =
    case messagesColumn of
        MessagesColumnNotVisible ->
            [hsx||]
        MessagesColumnVisible {..} ->
            [hsx|
                <div class="messages-list">
                    {renderMessageItems messageItems scheduledMessageItems}
                </div>
                <div class="message-input pt-0 pt-lg-2">
                    {renderSendMessageForm sendMessageForm}
                </div>
            |]

renderMessageItems :: [MessageItem] -> [ScheduledMessageItem] -> Html
renderMessageItems messageItems scheduledMessageItems =
    [hsx|
        <div class="list-group-flush d-flex flex-column-reverse flex-lg-column">
            {forEach messageItems renderMessageItem}
            {forEach scheduledMessageItems renderScheduledMessageItem}
            <div class="scroll-to-pinned d-none d-lg-block"></div>
        </div>
    |]

renderMessageItem :: MessageItem -> Html
renderMessageItem MessageItem {..} =
    [hsx|
        <div class="list-group-item flex-column align-items-start">
            <div class="d-flex w-100 justify-content-between">
                <h5 class="mb-1">{fromName}</h5>
                <span class="message-sent-at">
                    <time class="date-time" datetime={sentAt}>{sentAt}</time>
                </span>
            </div>
            <p class="message-body mb-1">{forEach entities renderEntity}</p>

            <div class="d-flex w-100 justify-content-between">
                <span class={statusClass}>{status}</span>
                <a href={linkButtonAction}
                    class={"btn btn-outline-primary btn-sm " <> linkButtonActiveClass}>
                    {linkButtonText}
                </a>
            </div>
        </div>
    |]

renderEntity :: Entity -> Html
renderEntity RecognizedEntity {..} =
    [hsx|
        <span class={classes} tabindex="0" role="button" data-toggle="tooltip" data-placement="top" title={tooltip}>
            {content}
        </span>
    |]
renderEntity UnrecognizedEntity {..} =
    [hsx|
        {content}
    |]

renderScheduledMessageItem :: ScheduledMessageItem -> Html
renderScheduledMessageItem NotStartedScheduledMessageItem {..} =
    [hsx|
        <div class="scheduled-message list-group-item flex-column align-items-start">
            <div class="d-flex w-100 justify-content-between">
                <h5 class="mb-1">Scheduled</h5>
                <span class="message-scheduled-for">
                    <time class="date-time" datetime={runsAt}>{runsAt}</time>
                </span>
            </div>
            <p class="message-body mb-1">{body}</p>

            <div class="d-flex w-100 justify-content-between">
                <span class="message-status scheduled">
                    Scheduled
                </span>
                
                <span>
                    <a href={editAction} class="btn btn-outline-primary btn-sm mr-2">Edit</a>

                    <form method="POST" action={updateAction} style="display: inline-block">
                        <input type="hidden" name="state" value="canceled">
                        <button type="submit" class="btn btn-outline-primary btn-sm">Cancel</button>
                    </form>
                </span>
            </div>
        </div>
    |]
renderScheduledMessageItem SuspendedScheduledMessageItem {..} =
    [hsx|
        <div class="scheduled-message-suspended list-group-item flex-column align-items-start">
            <div class="d-flex w-100 justify-content-between">
                <h5 class="mb-1">Suspended</h5>
                <span class="message-scheduled-for">
                    <time class="date-time" datetime={runsAt}>{runsAt}</time>
                </span>
            </div>
            <p class="message-body mb-1">{body}</p>

            <div class="d-flex w-100 justify-content-between">
                <span class="message-status text-warning">
                    Suspended
                </span>
                <span>
                    <a href={editAction} class="btn btn-outline-primary btn-sm mr-2">Edit</a>

                    <form method="POST" action={updateAction} class="mr-2" style="display: inline-block">
                        <input type="hidden" name="state" value="not_started">
                        <button type="submit" class="btn btn-outline-primary btn-sm">Resume</button>
                    </form>

                    <form method="POST" action={updateAction} style="display: inline-block">
                        <input type="hidden" name="state" value="canceled">
                        <button type="submit" class="btn btn-outline-primary btn-sm">Cancel</button>
                    </form>
                </span>
            </div>
        </div>
    |]
renderScheduledMessageItem EditScheduledMessageForm {..} =
    [hsx|
        <div class="edit-scheduled-message list-group-item flex-column align-items-start">
            <div class="d-flex w-100 justify-content-between">
                <h5 class="mb-3">Edit Message</h5>
                <span class="message-scheduled-for">
                    <time class="date-time" datetime={runsAt}>{runsAt}</time>
                </span>
            </div>

            <form method="POST" action={saveAction} class="edit-form">
                <div class="mb-3">
                    <textarea type="text" name="body" class="form-control edit-scheduled-message-form" rows="3" value={body}>
                        {body}
                    </textarea>
                </div>
                
                <div class="d-flex w-100 justify-content-end">
                    <button class="btn btn-primary btn btn-primary btn-sm">Save</button>
                    <a href={cancelAction} class="btn btn-secondary btn-sm ml-2">Cancel</a>
                </div>
            </form>
        </div>
    |]

renderSendMessageForm :: SendMessageForm -> Html
renderSendMessageForm SendMessageForm {..} =
    [hsx|
        <form method="POST" action={CommunicationsSendPhoneMessageAction} class="new-form" data-disable-javascript-submission="false">  
            <input type="hidden" name="toId" class="form-control" value={toPhoneNumberId}>

            <div class="input-group">
                <textarea class="message-body-input form-control" name="body" rows="3">
                </textarea>

                <div class="input-group-append">
                    <button class="btn btn-primary">Send</button>
                </div>
            </div>
        </form>
    |]

renderTimecardsColumn :: TimecardsColumn -> Html
renderTimecardsColumn timecardsColumn =
    [hsx|
        <div class="timecards-column">
            {renderTimecardsColumn' timecardsColumn}
        </div>
    |]

renderTimecardsColumn' :: TimecardsColumn -> Html
renderTimecardsColumn' timecardsColumn =
    case timecardsColumn of
        TimecardList {..} ->
            forEach timecardBlocks renderTimecardBlock
        NewTimecardEntry {..} ->
            renderTimecardEntryForm timecardEntryForm
        EditTimecardEntry {..} ->
            renderTimecardEntryForm timecardEntryForm
        EditModifiedTimecardEntry {..} ->
            renderTimecardEntryForm timecardEntryForm

renderTimecardBlock :: TimecardBlock -> Html
renderTimecardBlock TimecardBlock {..} =
    [hsx|
        <div class="card mb-4">
            <div class="card-body">
                <div class="d-flex justify-content-between mb-2">
                    <h5 class="card-title">Timecard for week of {weekOf}</h5>
                    <div>{renderTimecardStatus status}</div>
                </div>
                <div class="mb-4">
                    {renderTimecardActions actions}
                </div>
            
                <div>
                    <ul class="list-group">
                        {forEach entryCards renderTimecardEntryCard}
                    </ul>
                </div>
            </div>
        </div>
    |]

renderTimecardStatus :: TimecardStatus -> Html
renderTimecardStatus TimecardStatus {..} =
    [hsx|
        <span class={statusClasses}>
            {statusLabel}
        </span>
    |]

renderTimecardActions :: TimecardActions -> Html
renderTimecardActions timecardActions =
    case timecardActions of
        TimecardInProgress -> [hsx||]
        TimecardReadyForReview {..} ->
            [hsx|
                <form action={CommunicationsCreateTimecardReview} method="post">
                    <input type="hidden" name="selectedPersonId" value={selectedPersonId} />
                    <input type="hidden" name="timecardId" value={timecardId} />
                    <input type="submit" class="btn btn-outline-primary col-12" value="Send for Review">
                </form>
            |]
        TimecardUnderReview {..} ->
            [hsx|
                <a href={reviewAction}>Timecard Review Link</a>
            |]
        TimecardSigned -> [hsx||]

renderTimecardEntryCard :: TimecardEntryCard -> Html
renderTimecardEntryCard TimecardEntryCard {..} =
    [hsx|
        <div class="card mb-4">
            <h5 class="card-header">{dayOfWeek'} - {date}</h5>

            <div class="card-body">
                <h5 class="card-title">{jobName}</h5>
                <p class="card-text">{nl2br invoiceTranslation}</p>
                <a href={editAction} class="btn btn-primary">Edit</a>

                <form method="POST" action={deleteAction} style="display: inline-block" class="ml-2">
                    <button type="submit" class="btn btn-outline-primary">Delete</button>
                </form>
            </div>
        </div>
    |]

renderTimecardEntryForm :: TimecardEntryForm -> Html
renderTimecardEntryForm TimecardEntryForm {..} =
    [hsx|
        <form method="POST" action={submitAction} class="edit-form m-2" data-disable-javascript-submission="false"> 
            <div class="form-group">
                <label>Date</label>
                <input type="date" name="date" class={"custom-date-input form-control " <> dateInvalidClass} value={date}>
                {renderFieldError dateError}
            </div>
            
            <div class="form-group">
                <label>Job Name</label>
                <input type="text" name="jobName" placeholder="" class={"form-control " <> jobNameInvalidClass} value={jobName}>
                {renderFieldError jobNameError}
            </div>

            <div class="form-row">
                <div class="col form-group">
                    <label>Clock In</label>
                    <input type="text" name="clockedInAt" placeholder="" class={"form-control flatpickr-time-input " <> clockedInAtInvalidClass} value={clockedInAt}>
                    {renderFieldError clockedInAtError}
                </div>

                <div class="col form-group">
                    <label>Clock Out</label>
                    <input type="text" name="clockedOutAt" placeholder="" class={"form-control flatpickr-time-input " <> clockedOutAtInvalidClass} value={clockedOutAt}>
                    {renderFieldError clockedOutAtError}
                </div>

                <div class="col form-group">
                    <label>Lunch (mins)</label>
                    <input type="text" name="lunchDuration" placeholder="" class={"form-control " <> lunchDurationInvalidClass} value={lunchDuration}>
                    {renderFieldError lunchDurationError}
                </div>
            </div>
            
            <div class="form-group">
                <label>Hours Worked</label>
                <input type="text" name="hoursWorked" placeholder="" class={"form-control " <> hoursWorkedInvalidClass} value={hoursWorked}>
                {renderFieldError hoursWorkedError}
            </div>
            
            <div class="form-group">
                <label>Work Done</label>
                <textarea name="workDone" class={"work-done-input form-control " <> workDoneInvalidClass}>
                    {workDone}
                </textarea>
                {renderFieldError workDoneError}
            </div>
            
            <div class="form-group">
                <label>Invoice Translation</label>
                <textarea name="invoiceTranslation" class={"invoice-translation-input form-control " <> invoiceTranslationInvalidClass}>
                    {invoiceTranslation}
                </textarea>
                {renderFieldError invoiceTranslationError}
            </div>
            
            <input type="hidden" name="selectedMessageIds" value={selectedMessageIdsParam}>
            <input type="hidden" name="selectedPersonId" value={selectedPersonIdParam}>
                
            <button class="btn btn-primary">{submitLabel}</button>
            <a href={cancelAction} class="btn btn-secondary ml-2" role="button">Cancel</a>
        </form>
    |]

renderFieldError :: Maybe Violation -> Html
renderFieldError Nothing = [hsx||]
renderFieldError (Just violation) =
    [hsx| 
        <div class="invalid-feedback">{unwrapViolation violation}</div>
    |]

unwrapViolation :: Violation -> Html
unwrapViolation violation =
    case violation of
        TextViolation errorMessage -> [hsx| {errorMessage} |]
        HtmlViolation errorMessage -> H.preEscapedToHtml errorMessage

renderColumnNavigation :: ColumnNavigation -> Html
renderColumnNavigation ColumnNavigation {..} =
    [hsx|
        <ul class="column-nav p-0 mb-2 d-flex d-lg-none">
            <li class="column-nav-item flex-even d-flex justify-content-center">
                <a class={"column-nav-link text-center " <> peopleLinkClass} href={peopleAction}>People</a>
            </li>
            <li class="column-nav-item flex-even d-flex justify-content-center">
                <a class={"column-nav-link text-center " <> messagesLinkClass} href={messagesAction}>Messages</a>
            </li>
            <li class="column-nav-item flex-even d-flex justify-content-center">
                <a class={"column-nav-link text-center " <> timecardsLinkClass} href={timecardsAction}>Timecards</a>
            </li>
        </ul>
    |]

styles :: Html
styles =
    [hsx|
        <style>
            @media only screen and (min-width: 992px) {
                :root {
                    --column-nav-height: 0rem;
                    --mobile-browser-bar-height: 0rem;
                }

                .timecards-column {
                    width: 31rem;
                }
            }

            @media only screen and (max-width: 992px) {
                :root {
                    --column-nav-height: 3rem;
                    --mobile-browser-bar-height: 8rem;
                }
            }

            :root {
                --section-nav-height: 7.25rem;
                --total-nav-height: calc(var(--section-nav-height) + var(--column-nav-height) + var(--mobile-browser-bar-height));
                --message-input-height: 7rem;
                --screen-height: 100vh;
            }

            .column-nav {
                height: var(--column-nav-height);
            }

            .column-nav-item {
                list-style-type: none;
            }

            .column-nav-link {
                width: 100%;
                line-height: 2.5rem;
            }

            .people-list {
                height: calc(var(--screen-height) - var(--total-nav-height));
                min-width: 18.75rem;
                overflow-y: scroll;
            }

            .messages-list {
                height: calc(var(--screen-height) - calc(var(--total-nav-height) + var(--message-input-height)));
                overflow-x: hidden;
                overflow-y: scroll;
            }

            .message-input {
                height: var(--message-input-height);
            }

            .timecards-column {
                height: calc(var(--screen-height) - var(--total-nav-height));
                overflow-y: scroll;
            }

            .edit-scheduled-message {
                background-color: whitesmoke;
                height: 12.5rem;
            }

            .edit-scheduled-message-form {
                font-size: .9rem;
            }

            .message-body-input {
                resize: none;
            }

            .message-body {
                white-space: pre-line;
                word-wrap: break-word;
                word-break: break-word;
            }

            .entity.job-name {
                background-color: #d1e3ff;
            }

            .entity.hours-worked {
                background-color: #ffddd1;
            }

            .entity.clocked-in-at {
                background-color: #d1ffd4;
            }

            .entity.clocked-out-at {
                background-color: #d1d6ff;
            }

            .entity.time-on-task {
                background-color: #fdd1ff;
            }

            .entity.work-done {
                background-color: #faffd1;
            }

            .message-sent-at {
                font-size: 80%;
                color: darkgray;
            }

            .message-status {
                font-size: 80%;
            }

            .message-status.delivered {
                color: green;
            }

            .message-status.received {
                color: rgb(26, 124, 236);
            }

            .message-status.failed {
                color: red;
            }

            .message-status.sending {
                color: darkgray;
            }

            .message-status.scheduled {
                color: blueviolet;
            }

            .scheduled-message {
                background-color: whitesmoke;
            }

            .scheduled-message-suspended {
                background-color: #fff5ee;
            }

            .message-scheduled-for {
                font-size: 80%;
                color: black;
            }

            .work-done-input {
                min-height: 6.9rem;
            }

            .invoice-translation-input {
                min-height: 6.9rem;
            }

            .flex-even {
                flex: 1;
            }

            body {
                overflow: hidden;
            }
        </style>

        {removeScrollbars}
    |]
