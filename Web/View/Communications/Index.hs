module Web.View.Communications.Index where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.People.Person as Person
import qualified Application.People.View as V
import qualified Application.Timecard.View as V
import qualified Application.Twilio.Query as Twilio.Query
import Web.View.Navigation.People
import Web.View.Navigation.Section (Section (Communications), renderSectionNavigation)
import Web.View.Prelude hiding (Page)
import Web.View.Service.Style (removeScrollbars)
import Web.View.Service.Time (formatDateTime, formatDay)
import Web.View.Timecards.Status

data IndexView = IndexView
    { people :: ![V.Person]
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

data Column
    = People
    | Messages
    | Timecards
    deriving (Eq, Show)

instance View IndexView where
    html view = renderPage $ buildPage view

renderPage :: Page -> Html
renderPage Page {..} =
    [hsx|
        <!-- Desktop version of the page. -->
        <div class="d-none d-xl-block">
            <div class="communications-page d-flex flex-column">
                {renderSectionNavigation Communications selectedPerson}
                
                <div class="d-flex flex-row">
                    <div class="mr-3 d-flex flex-column">
                        {renderPeopleNavigation peopleNavigation}
                    </div>
                    <div class="flex-grow-1 d-flex flex-column">
                        {renderMessagesColumn messagesColumn}
                    </div>
                    <div class="ml-3 d-flex flex-column">
                        {renderTimecardsColumn timecardColumn}
                    </div>
                </div>
            </div>
        </div>
        
        <!-- Mobile version of the page. -->
        <div class="d-block d-xl-none">
            <div class="communications-page d-flex flex-column">
                <div id="people" class="d-flex flex-column">
                    {renderSectionNavigation Communications selectedPerson}
                    {renderPeopleNavigation peopleNavigation}
                    {renderColumnNavigation People}
                    <div class="browser-nav bg-light"></div>
                </div>
                <div id="messages" class="d-flex flex-column">
                    {renderSectionNavigation Communications selectedPerson}
                    {renderMobileMessagesColumn messagesColumn}
                    {renderColumnNavigation Messages}
                    <div class="browser-nav bg-light"></div>
                </div>
                <div id="timecards" class="d-flex flex-column">
                    {renderSectionNavigation Communications selectedPerson}
                    {renderTimecardsColumn timecardColumn}
                    {renderColumnNavigation Timecards}
                    <div class="browser-nav bg-light"></div>
                </div>
            </div>
        </div>

        {styles}
    |]

buildPage :: IndexView -> Page
buildPage view =
    Page
        { selectedPerson = selectedPerson
        , peopleNavigation =
            buildPeopleNavigation
                BadgesVisible
                CommunicationsPersonSelectionAction
                (Anchor "messages")
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
        , sentAt = formatDateTime $ get #createdAt message
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
            [hsx||]
        MessagesColumnVisible {..} ->
            [hsx|
                <div class="messages-list scroll-to-end">
                    {renderMessageItems messageItems scheduledMessageItems}
                </div>
                <div class="message-input pt-2 mb-2">
                    {renderSendMessageForm sendMessageForm}
                </div>
            |]

renderMobileMessagesColumn :: MessagesColumn -> Html
renderMobileMessagesColumn messagesColumn =
    case messagesColumn of
        MessagesColumnNotVisible ->
            [hsx||]
        MessagesColumnVisible {..} ->
            [hsx|
                <div class="message-input pb-2">
                    {renderSendMessageForm sendMessageForm}
                </div>
                <div class="messages-list">
                    {renderMessageItems (reverse messageItems) (reverse scheduledMessageItems)}
                </div>
            |]

renderMessageItems :: [MessageItem] -> [ScheduledMessageItem] -> Html
renderMessageItems messageItems scheduledMessageItems =
    [hsx|
        <div class="list-group-flush">
            {forEach messageItems renderMessageItem}
            {forEach scheduledMessageItems renderScheduledMessageItem}
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
                <a href={pathTo linkButtonAction <> "#timecards"}
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

renderTimecardsColumn :: TimecardColumn -> Html
renderTimecardsColumn timecardColumn =
    [hsx|
        <div class="timecards-block">
            {renderTimecardsColumn' timecardColumn}
        </div>
    |]

renderTimecardsColumn' :: TimecardColumn -> Html
renderTimecardsColumn' timecardColumn =
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
            <h5 class="card-header">{dayOfWeek'} - {date}</h5>

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
        <form method="POST" action={submitAction} class="edit-form m-2" data-disable-javascript-submission="false"> 
            <div class="form-group">
                <label>Date</label>
                <input type="date" name="date" class={"form-control " <> dateInvalidClass} value={date}>
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

renderFieldError :: Maybe Text -> Html
renderFieldError Nothing = [hsx||]
renderFieldError (Just errorMessage) =
    [hsx| 
        <div class="invalid-feedback">{errorMessage}</div>
    |]

renderColumnNavigation :: Column -> Html
renderColumnNavigation currentColumn =
    [hsx|
        <ul class="bottom-nav m-0 p-0 border-top bg-light d-flex">
            <li class="bottom-nav-item flex-even border-right d-flex justify-content-center">
                <a class={"bottom-nav-link text-center " <> linkClass People} href="#people">People</a>
            </li>
            <li class="bottom-nav-item flex-even d-flex justify-content-center">
                <a class={"bottom-nav-link text-center " <> linkClass Messages} href="#messages">Messages</a>
            </li>
            <li class="bottom-nav-item flex-even border-left d-flex justify-content-center">
                <a class={"bottom-nav-link text-center " <> linkClass Timecards} href="#timecards">Timecards</a>
            </li>
        </ul>
    |]
  where
    linkClass column = if column == currentColumn then "text-dark" :: Text else "text-muted"

styles :: Html
styles =
    [hsx|
        <style>
            @media only screen and (min-width: 1200px) {
                :root {
                    --bottom-nav-height: 0rem;
                    --browser-nav-height: 0rem;
                }

                .timecards-block {
                    width: 31rem;
                }
            }

            @media only screen and (max-width: 1200px) {
                :root {
                    --bottom-nav-height: 3rem;
                    --browser-nav-height: 7rem;
                }
            }

            :root {
                --top-nav-height: 7.25rem;
                --total-nav-height: calc(var(--top-nav-height) + var(--bottom-nav-height) + var(--browser-nav-height));
                --message-input-height: 5.8rem;
                --screen-height: 100vh;
            }

            .communications-page {
                height: var(--screen-height);
                overflow: hidden;
            }

            .top-nav {
                height: var(--top-nav-height);
            }

            .bottom-nav {
                height: var(--bottom-nav-height);
            }

            .bottom-nav-item {
                list-style-type: none;
            }

            .bottom-nav-link {
                width: 100%;
                line-height: 2.5rem;
            }

            .browser-nav {
                height: var(--browser-nav-height);
            }

            .people-list {
                height: calc(var(--screen-height) - var(--total-nav-height));
                min-width: 18.75rem;
                overflow-y: scroll;
            }

            .messages-list {
                height: calc(var(--screen-height) - calc(var(--total-nav-height) + var(--message-input-height)));
                overflow-y: scroll;
            }

            .message-input {
                height: var(--message-input-height);
            }

            .timecards-block {
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
