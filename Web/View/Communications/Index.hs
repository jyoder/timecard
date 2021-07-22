module Web.View.Communications.Index where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Timecard.View as V
import qualified Application.Twilio.Query as Twilio.Query
import Web.View.Navigation.People
import Web.View.Navigation.Section (Section (Communications), renderSectionNavigation)
import Web.View.Prelude hiding (Page)
import Web.View.Service.Style (removeScrollbars)
import Web.View.Service.Time (formatDay)
import Web.View.Timecards.Status

data IndexView = IndexView
    { people :: ![Person]
    , personSelection :: !PersonSelection
    }

data PersonSelection
    = NoPersonSelected
    | PersonSelected
        { selectedPerson :: !Person
        , messages :: ![Twilio.Query.Row]
        , toPhoneNumber :: !PhoneNumber
        , scheduledMessages :: ![SendMessageAction.T]
        , editingScheduledMessage :: !Bool
        , newMessage :: !TwilioMessage
        , personActivity :: !PersonActivity
        }

data PersonActivity
    = SendingMessage
        { timecards :: ![V.Timecard]
        }
    | WorkingOnTimecardEntry
        { timecardEntry :: !TimecardEntry
        , selectedMessages :: ![Twilio.Query.Row]
        , timecardActivity :: TimecardActivity
        }

data TimecardActivity
    = CreatingEntry
    | EditingEntry
    | EditingModifiedEntry
    deriving (Eq)

data Page = Page
    { selectedPerson :: !(Maybe Person)
    , peopleNavigation :: !(PeopleNavigation CommunicationsController)
    , messagesColumn :: !MessagesColumn
    , timecardColumn :: !TimecardColumn
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
    , body :: !Text
    , statusClass :: !Text
    , status :: !Text
    , linkButtonActiveClass :: !Text
    , linkButtonText :: !Text
    , linkButtonAction :: !CommunicationsController
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

data TimecardColumn
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
    }
    deriving (Eq, Show)

data TimecardEntryForm = TimecardEntryForm
    { date :: !Text
    , dateInvalidClass :: !Text
    , dateError :: !(Maybe Text)
    , jobName :: !Text
    , jobNameInvalidClass :: !Text
    , jobNameError :: !(Maybe Text)
    , clockedInAt :: !Text
    , clockedInAtInvalidClass :: !Text
    , clockedInAtError :: !(Maybe Text)
    , clockedOutAt :: !Text
    , clockedOutAtInvalidClass :: !Text
    , clockedOutAtError :: !(Maybe Text)
    , lunchDuration :: !Text
    , lunchDurationInvalidClass :: !Text
    , lunchDurationError :: !(Maybe Text)
    , hoursWorked :: !Text
    , hoursWorkedInvalidClass :: !Text
    , hoursWorkedError :: !(Maybe Text)
    , workDone :: !Text
    , workDoneInvalidClass :: !Text
    , workDoneError :: !(Maybe Text)
    , invoiceTranslation :: !Text
    , invoiceTranslationInvalidClass :: !Text
    , invoiceTranslationError :: !(Maybe Text)
    , selectedMessageIdsParam :: !Text
    , selectedPersonIdParam :: !Text
    , submitLabel :: !Text
    , submitAction :: !CommunicationsController
    , cancelAction :: !CommunicationsController
    }
    deriving (Eq, Show)

instance View IndexView where
    html view = renderPage $ buildPage view

renderPage :: Page -> Html
renderPage Page {..} =
    [hsx|
            {renderSectionNavigation Communications selectedPerson}

            <div class="row align-items start">
                {renderPeopleNavigation peopleNavigation}
                {renderMessagesColumn messagesColumn}
                {renderTimecardColumn timecardColumn}
            </div>

            {styles}
        |]

buildPage :: IndexView -> Page
buildPage view =
    Page
        { selectedPerson = selectedPerson
        , peopleNavigation =
            buildPeopleNavigation
                CommunicationsPersonSelectionAction
                selectedPerson
                (get #people view)
        , messagesColumn = buildMessagesColumn view
        , timecardColumn = buildTimecardColumn view
        }
  where
    selectedPerson = case get #personSelection view of
        NoPersonSelected -> Nothing
        PersonSelected {..} -> Just selectedPerson

buildMessagesColumn :: IndexView -> MessagesColumn
buildMessagesColumn IndexView {..} =
    case personSelection of
        NoPersonSelected -> MessagesColumnNotVisible
        PersonSelected {..} ->
            MessagesColumnVisible
                { messageItems = buildMessageItems selectedPerson personActivity messages
                , scheduledMessageItems =
                    buildScheduledMessageItem
                        editingScheduledMessage
                        (get #id selectedPerson)
                        <$> scheduledMessages
                , sendMessageForm = buildSendMessageForm toPhoneNumber
                }

buildMessageItems :: Person -> PersonActivity -> [Twilio.Query.Row] -> [MessageItem]
buildMessageItems selectedPerson personActivity messages =
    let selectedMessageIds = case personActivity of
            SendingMessage {..} -> []
            WorkingOnTimecardEntry {..} -> get #id <$> selectedMessages
     in buildMessageItem selectedPerson personActivity selectedMessageIds <$> messages

buildMessageItem ::
    Person ->
    PersonActivity ->
    [Id TwilioMessage] ->
    Twilio.Query.Row ->
    MessageItem
buildMessageItem selectedPerson personActivity selectedMessageIds message =
    MessageItem
        { fromName = get #fromFirstName message <> " " <> get #fromLastName message
        , sentAt = show $ get #createdAt message
        , body = get #body message
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
    toggledMessageIds = show <$> dummyMessageId : if isSelected then excludeMessageId else includeMessageId
    isSelected = messageId `elem` selectedMessageIds
    excludeMessageId = filter (/= messageId) selectedMessageIds
    includeMessageId = messageId : selectedMessageIds
    messageId = get #id message
    -- TODO: this is a workaround to deal with a bug in AutoRoute:
    -- https://github.com/digitallyinduced/ihp/issues/971
    dummyMessageId = "00000000-0000-0000-0000-000000000000"

messageStatusClass :: Twilio.Query.Status -> Text
messageStatusClass status =
    case status of
        Twilio.Query.Delivered -> "message-status delivered"
        Twilio.Query.Received -> "message-status received"
        Twilio.Query.Failed -> "message-status failed"
        _ -> "message-status sending"

buildScheduledMessageItem :: Bool -> Id Person -> SendMessageAction.T -> ScheduledMessageItem
buildScheduledMessageItem
    editingScheduledMessage
    selectedPersonId
    scheduledMessage
        | editingScheduledMessage =
            buildEditScheduledMessageForm selectedPersonId scheduledMessage
        | get #state scheduledMessage == ActionRunState.suspended =
            buildSuspendedScheduledMessageItem scheduledMessage
        | otherwise = buildNotStartedScheduledMessageItem scheduledMessage

buildSuspendedScheduledMessageItem :: SendMessageAction.T -> ScheduledMessageItem
buildSuspendedScheduledMessageItem scheduledMessage =
    SuspendedScheduledMessageItem
        { runsAt = show $ get #runsAt scheduledMessage
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
        { runsAt = show $ get #runsAt scheduledMessage
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
        { runsAt = show $ get #runsAt scheduledMessage
        , body = get #body scheduledMessage
        , sendMessageActionId = show $ get #id scheduledMessage
        , saveAction = saveAction
        , cancelAction = cancelAction
        }
  where
    saveAction = CommunicationsUpdateScheduledMessageAction (get #id scheduledMessage)
    cancelAction = CommunicationsPersonSelectionAction selectedPersonId

buildSendMessageForm :: PhoneNumber -> SendMessageForm
buildSendMessageForm toPhoneNumber =
    SendMessageForm
        { toPhoneNumberId = show $ get #id toPhoneNumber
        }

buildTimecardColumn :: IndexView -> TimecardColumn
buildTimecardColumn IndexView {..} =
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
        }

buildTimecardEntryForm ::
    Person ->
    [Twilio.Query.Row] ->
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
            , cancelAction = CommunicationsPersonSelectionAction (get #id selectedPerson)
            }
      where
        hasErrorFor = isJust . errorFor
        errorFor fieldName = snd <$> find (\(name, errorMessage) -> name == fieldName) annotations
        annotations = timecardEntry |> get #meta |> get #annotations
        invalidClass = "is-invalid"
        workDone = assembleMessageBodies (get #workDone timecardEntry) sortedMessages
        invoiceTranslations = assembleMessageBodies (get #invoiceTranslation timecardEntry) sortedMessages
        selectedPersonId = show $ get #id selectedPerson
        selectedMessageIdsParam = intercalate "," (show . get #id <$> sortedMessages)
        sortedMessages = sortBy (\m1 m2 -> get #createdAt m1 `compare` get #createdAt m2) selectedMessages
        submitAction = if timecardActivity == CreatingEntry then createAction else updateAction
        createAction = CommunicationsCreateTimecardEntryAction
        updateAction = CommunicationsUpdateTimecardEntryAction $ get #id timecardEntry

assembleMessageBodies :: Text -> [Twilio.Query.Row] -> Text
assembleMessageBodies existingText messages =
    if existingText == ""
        then intercalate "\n\n" (get #body <$> messages)
        else existingText

renderMessagesColumn :: MessagesColumn -> Html
renderMessagesColumn messagesColumn =
    case messagesColumn of
        MessagesColumnNotVisible ->
            [hsx|
                <div class="col-6"></div>
            |]
        MessagesColumnVisible {..} ->
            [hsx|
                <div class="col-6">
                    {renderMessageItems messageItems scheduledMessageItems}
                    <div class="message-input">
                        {renderSendMessageForm sendMessageForm}
                    </div>
                </div>
            |]

renderMessageItems :: [MessageItem] -> [ScheduledMessageItem] -> Html
renderMessageItems messageItems scheduledMessageItems =
    [hsx|
        <div class="message-history list-group-flush">
            {forEach messageItems renderMessageItem}
            {forEach scheduledMessageItems renderScheduledMessageItem}
            <div class="scroll-pinned"></div>
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
            <p class="message-body mb-1">{body}</p>

            <div class="d-flex w-100 justify-content-between">
                <span class={statusClass}>{status}</span>
                <a href={linkButtonAction}
                    data-turbolinks="false"
                    class={"btn btn-outline-primary btn-sm " <> linkButtonActiveClass}>
                    {linkButtonText}
                </a>
            </div>
        </div>
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

            <form method="POST" 
                action={saveAction}
                class="edit-form"
                data-disable-javascript-submission="false">
                
                <div class="mb-3">
                    <textarea
                        type="text"
                        name="body"
                        class="form-control edit-scheduled-message-form"
                        rows="3"
                        value={body}>
                        {body}
                    </textarea> 
                </div>
                
                <div>
                    <input
                        type="hidden"
                        name="id"
                        class="form-control"
                        value={sendMessageActionId}>
                </div>
                
                <div class="d-flex w-100 justify-content-end">
                    <button class="btn btn-primary btn btn-primary btn-sm">
                        Save
                    </button>
                
                    <a href={cancelAction} class="btn btn-secondary btn-sm ml-2">
                        Cancel
                    </a>
                </div>
            </form>
        </div>
    |]

renderSendMessageForm :: SendMessageForm -> Html
renderSendMessageForm SendMessageForm {..} =
    [hsx|
        <div class="message-input">
            <form 
                method="POST"
                action={CommunicationsSendPhoneMessageAction}
                id="send-message-form" 
                class="new-form"
                data-disable-javascript-submission="false">
                
                <div class="form-group" id="form-group-twilioMessage_toId">
                    <input 
                        type="hidden"
                        name="toId"
                        id="twilioMessage_toId"
                        class="form-control"
                        value={toPhoneNumberId}>
                </div>

                <div class="input-group">
                    <textarea 
                        class="form-control"
                        id="twilioMessage_body"
                        name="body"
                        rows="3">
                    </textarea>

                    <div class="input-group-append">
                        <button class="btn btn-primary">Send</button>
                    </div>
                </div>
            </form>
        </div>
    |]

renderTimecardColumn :: TimecardColumn -> Html
renderTimecardColumn timecardColumn =
    [hsx|
        <div class="timecard-column col-4">
            {renderTimecardColumn' timecardColumn}
        </div>
    |]

renderTimecardColumn' :: TimecardColumn -> Html
renderTimecardColumn' timecardColumn =
    case timecardColumn of
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
            <h5 class="card-header">
                {dayOfWeek'} - {date}
            </h5>

            <div class="card-body">
                <h5 class="card-title">{jobName}</h5>
                <p class="card-text">{invoiceTranslation}</p>
                <a href={editAction} class="btn btn-primary">Edit</a>
            </div>
        </div>
    |]

renderTimecardEntryForm :: TimecardEntryForm -> Html
renderTimecardEntryForm TimecardEntryForm {..} =
    [hsx|
        <form
            method="POST" 
            action={submitAction} 
            id="timecard-form" 
            class="edit-form" 
            data-disable-javascript-submission="false">
            
            <div class="form-group" id="form-group-timecardEntry_date">
                <label class="" for="timecardEntry_date">
                    Date
                </label>

                <input 
                    type="date" 
                    name="date" 
                    id="timecardEntry_date" 
                    class={"form-control " <> dateInvalidClass} 
                    value={date}>

                {renderFieldError dateError}
            </div>
            
            <div class="form-group" id="form-group-timecardEntry_jobName">
                <label class="" for="timecardEntry_jobName">
                    Job Name
                </label>

                <input 
                    type="text"
                    name="jobName"
                    placeholder=""
                    id="timecardEntry_jobName"
                    class={"form-control " <> jobNameInvalidClass}
                    value={jobName}>

                {renderFieldError jobNameError}
            </div>

            <div class="form-row">
                <div class="col form-group" id="form-group-timecardEntry_clockedInAt">
                    <label class="" for="timecardEntry_clockedInAt">
                        Clock In
                    </label>

                    <input 
                        type="text"
                        name="clockedInAt"
                        placeholder=""
                        id="timecardEntry_clockedInAt"
                        class={"form-control flatpickr-time-input " <> clockedInAtInvalidClass}
                        value={clockedInAt}>

                    {renderFieldError clockedInAtError}
                </div>

                <div class="col form-group" id="form-group-timecardEntry_clockedOutAt">
                    <label class="" for="timecardEntry_clockedOutAt">
                        Clock Out
                    </label>

                    <input 
                        type="text"
                        name="clockedOutAt"
                        placeholder=""
                        id="timecardEntry_clockedOutAt"
                        class={"form-control flatpickr-time-input " <> clockedOutAtInvalidClass}
                        value={clockedOutAt}>

                    {renderFieldError clockedOutAtError}
                </div>

                <div class="col form-group" id="form-group-timecardEntry_lunchDuration">
                    <label class="" for="timecardEntry_lunchDuration">
                        Lunch (mins)
                    </label>

                    <input 
                        type="text"
                        name="lunchDuration"
                        placeholder=""
                        id="timecardEntry_lunchDuration"
                        class={"form-control " <> lunchDurationInvalidClass}
                        value={lunchDuration}>

                    {renderFieldError lunchDurationError}
                </div>
            </div>
            
            <div class="form-group" id="form-group-timecardEntry_hoursWorked">
                <label class="" for="timecardEntry_hoursWorked">
                    Hours Worked
                </label>

                <input 
                    type="text"
                    name="hoursWorked"
                    placeholder=""
                    id="timecardEntry_hoursWorked"
                    class={"form-control " <> hoursWorkedInvalidClass}
                    value={hoursWorked}>

                {renderFieldError hoursWorkedError}
            </div>
            
            <div id="form-group-timecardEntry_workDone" class="form-group">
                <label for="timecardEntry_workDone">
                    Work Done
                </label>

                <textarea 
                    id="timecardEntry_workDone" 
                    name="workDone" 
                    class={"form-control " <> workDoneInvalidClass}>
                    {workDone}
                </textarea>

                {renderFieldError workDoneError}
            </div>
            
            <div id="form-group-timecardEntry_invoiceTranslation" class="form-group">
                <label for="timecardEntry_invoiceTranslation">
                    Invoice Translation
                </label>
                
                <textarea
                    id="timecardEntry_invoiceTranslation"
                    name="invoiceTranslation"
                    class={"form-control " <> invoiceTranslationInvalidClass}>
                    {invoiceTranslation}
                </textarea>

                {renderFieldError invoiceTranslationError}
            </div>
            
            <input 
                type="hidden"
                name="selectedMessageIds"
                id="selectedMessageIds"
                class="form-control"
                value={selectedMessageIdsParam}>
            
            <input
                type="hidden"
                name="selectedPersonId"
                id="selectedPersonId"
                class="form-control"
                value={selectedPersonIdParam}>
                
            <button class="btn btn-primary">
                {submitLabel}
            </button>
            
            <a href={cancelAction} class="btn btn-secondary ml-2" role="button">
                Cancel
            </a>
        </form>
    |]

renderFieldError :: Maybe Text -> Html
renderFieldError Nothing = [hsx||]
renderFieldError (Just errorMessage) =
    [hsx| 
        <div class="invalid-feedback">{errorMessage}</div>
    |]

styles :: Html
styles =
    [hsx|
    <style>
        .people-column {
            height: calc(100vh - 150px);
            overflow-y: scroll;
        }

        .message-history {
            height: calc(100vh - 270px);
            overflow-y: scroll;
        }

        .message-input {
            height: 100px;
            padding: 0px;
            margin: 0px;
        }

        .message-body {
            white-space: pre-line;
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

        .timecard-column {
            height: calc(100vh - 150px);
            overflow-y: scroll;
        }

        .edit-scheduled-message {
            background-color: whitesmoke;
            height: 200px;
        }

        .edit-scheduled-message-form {
            font-size: .9rem;
        }

        #twilioMessage_body {
            resize: none;
        }

        #timecardEntry_workDone {
            height: 123px;
        }

        #timecardEntry_invoiceTranslation {
            height: 123px;
        }
    </style>

    {removeScrollbars}
|]
