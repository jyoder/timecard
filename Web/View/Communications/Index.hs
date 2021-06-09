module Web.View.Communications.Index where

import qualified Application.Service.SendMessageAction as SendMessageAction
import Data.ByteString.UTF8 (toString)
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import IHP.View.TimeAgo as TO
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.View.Navigation (Section (Communications), renderNavigation)
import Web.View.Prelude
import Web.View.Service.Style (removeScrollbars)
import Web.View.Service.Time (weekday)

data IndexView = IndexView
    { people :: ![Person]
    , personSelection :: !PersonSelection
    }

data PersonSelection
    = NoPersonSelected
    | PersonSelected
        { selectedPerson :: !Person
        , messages :: ![Message]
        , toPhoneNumber :: !PhoneNumber
        , scheduledMessages :: ![SendMessageAction.T]
        , newMessage :: !TwilioMessage
        , personActivity :: !PersonActivity
        }

data PersonActivity
    = SendingMessage
        { timecardEntries :: ![TimecardEntry]
        }
    | WorkingOnTimecardEntry
        { timecardEntry :: !TimecardEntry
        , selectedMessages :: ![Message]
        , timecardActivity :: TimecardActivity
        }

data TimecardActivity
    = CreatingEntry
    | EditingEntry
    | EditingModifiedEntry
    deriving (Eq)

data Message = Message
    { id :: !(Id TwilioMessage)
    , fromName :: !Text
    , toName :: !Text
    , createdAt :: !UTCTime
    , status :: !MessageStatus
    , body :: !Text
    }

data MessageStatus
    = Accepted
    | Scheduled
    | Queued
    | Sending
    | Sent
    | Receiving
    | Received
    | Delivered
    | Undelivered
    | Failed
    | Read
    deriving (Show)

instance FromRow Message where
    fromRow =
        Message
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

instance FromField MessageStatus where
    fromField field maybeData =
        case maybeData of
            Just "accepted" -> pure Accepted
            Just "scheduled" -> pure Scheduled
            Just "queued" -> pure Queued
            Just "sending" -> pure Sending
            Just "sent" -> pure Sent
            Just "receiving" -> pure Receiving
            Just "received" -> pure Received
            Just "delivered" -> pure Delivered
            Just "undelivered" -> pure Undelivered
            Just "failed" -> pure Failed
            Just "read" -> pure Read
            Just _data -> returnError ConversionFailed field (toString _data)
            Nothing -> returnError UnexpectedNull field ""

instance View IndexView where
    html view =
        [hsx|
            {renderNavigation Communications}

            <div class="row align-items start">
                {renderPeopleColumn view}
                {renderMessagesColumn view}
                {renderTimecardColumn view}
            </div>

            {styles}
        |]

renderPeopleColumn :: IndexView -> Html
renderPeopleColumn IndexView {..} =
    [hsx|
        <div class="people-column col-2">
            <div class="list-group">
                {forEach people renderPerson'}
            </div>
        </div>        
    |]
  where
    renderPerson' = case personSelection of
        NoPersonSelected -> renderPerson False
        PersonSelected {..} ->
            let isSelected person = get #id person == get #id selectedPerson
             in (\person -> renderPerson (isSelected person) person)

renderMessagesColumn :: IndexView -> Html
renderMessagesColumn IndexView {..} =
    case personSelection of
        NoPersonSelected -> [hsx||]
        PersonSelected {..} ->
            let selectedMessageIds = case personActivity of
                    SendingMessage {..} -> []
                    WorkingOnTimecardEntry {..} -> get #id <$> selectedMessages
             in [hsx|
                <div class="col-6">
                    {renderMessages selectedPerson personActivity selectedMessageIds messages scheduledMessages}
                    <div class="message-input">
                        {renderSendMessageForm toPhoneNumber newMessage}
                    </div>
                </div>
            |]

renderTimecardColumn :: IndexView -> Html
renderTimecardColumn view =
    [hsx|
        <div class="timecard-column col-4">
            {renderTimecardBlock view}
        </div>
    |]

renderTimecardBlock :: IndexView -> Html
renderTimecardBlock IndexView {..} =
    case personSelection of
        NoPersonSelected -> [hsx||]
        PersonSelected {..} ->
            case personActivity of
                SendingMessage {..} ->
                    renderTimecardEntries
                        selectedPerson
                        timecardEntries
                WorkingOnTimecardEntry {..} ->
                    renderTimecardEntryForm
                        selectedPerson
                        selectedMessages
                        timecardActivity
                        timecardEntry

renderPerson :: Bool -> Person -> Html
renderPerson isSelected person =
    [hsx|
        <a
            href={PersonSelectionAction (get #id person)}
            class={"list-group-item " <> activeClass}
            aria-current={ariaCurrent}>
            {get #firstName person} {get #lastName person}
        </a>
    |]
  where
    activeClass = if isSelected then "active" else "" :: Text
    ariaCurrent = if isSelected then "true" else "false" :: Text

renderMessages ::
    Person ->
    PersonActivity ->
    [Id TwilioMessage] ->
    [Message] ->
    [SendMessageAction.T] ->
    Html
renderMessages
    selectedPerson
    personActivity
    selectedMessageIds
    messages
    scheduledMessages =
        [hsx|
        <div class="message-history list-group-flush">
            {forEach messages $ renderMessage selectedPerson personActivity selectedMessageIds}
            {forEach scheduledMessages renderScheduledMessage}
            <div class="scroll-pinned"></div>
        </div>
    |]

renderSendMessageForm :: PhoneNumber -> TwilioMessage -> Html
renderSendMessageForm phoneNumber newMessage =
    [hsx|
        <div class="message-input">
            <form 
                method="POST"
                action="/CreateOutgoingPhoneMessage"
                id="send-message-form" 
                class="new-form"
                data-disable-javascript-submission="false">
                
                <div class="form-group" id="form-group-twilioMessage_toId">
                    <input 
                        type="hidden"
                        name="toId"
                        id="twilioMessage_toId"
                        class="form-control"
                        value={show $ get #id phoneNumber}>
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

renderMessage :: Person -> PersonActivity -> [Id TwilioMessage] -> Message -> Html
renderMessage selectedPerson personActivity selectedMessageIds message =
    [hsx|
        <div
            class="list-group-item flex-column align-items-start">
            <div class="d-flex w-100 justify-content-between">
                <h5 class="mb-1">{fromName}</h5>
                <span class="message-sent-at">{renderSentAt message}</span>
            </div>
            <p class="message-body mb-1">{body}</p>

            <div class="d-flex w-100 justify-content-between">
                <span class={messageStatusClass'}>
                    {messageStatus}
                </span>
                <a href={nextAction}
                    data-turbolinks="false"
                    class={"btn btn-outline-primary btn-sm " <> activeClass}>
                    {linkButtonText}
                </a>
            </div>
        </div>
    |]
  where
    fromName = get #fromName message
    body = get #body message
    messageStatusClass' = messageStatusClass $ get #status message
    messageStatus = show $ get #status message
    activeClass = if isSelected then "active" else "" :: Text
    linkButtonText = if isSelected then "Unlink" else "Link" :: Text
    nextAction = case personActivity of
        SendingMessage {..} -> NewTimecardEntryAction {selectedMessageIds = toggledMessageIds, ..}
        WorkingOnTimecardEntry {..} -> case timecardActivity of
            CreatingEntry -> NewTimecardEntryAction {selectedMessageIds = toggledMessageIds, ..}
            EditingEntry ->
                EditModifiedTimecardEntryAction
                    { selectedMessageIds = toggledMessageIds
                    , timecardEntryId = get #id timecardEntry
                    , ..
                    }
            EditingModifiedEntry ->
                EditModifiedTimecardEntryAction
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

renderScheduledMessage :: SendMessageAction.T -> Html
renderScheduledMessage scheduledMessage =
    [hsx|
        <div
            class="scheduled-message list-group-item flex-column align-items-start">
            <div class="d-flex w-100 justify-content-between">
                <h5 class="mb-1">Scheduled</h5>
                <span class="message-scheduled-for">{renderScheduledFor scheduledMessage}</span>
            </div>
            <p class="message-body mb-1">{body}</p>

            <div class="d-flex w-100 justify-content-between">
                <span class="message-status scheduled">
                    Scheduled
                </span>
                <a href={cancelAction}
                    data-turbolinks="false"
                    class="btn btn-outline-primary btn-sm">
                    Cancel
                </a>
            </div>
        </div>
    |]
  where
    body = get #body scheduledMessage
    cancelAction = CancelScheduledMessageAction (get #id scheduledMessage)

renderTimecardEntries :: Person -> [TimecardEntry] -> Html
renderTimecardEntries selectedPerson timecardEntries =
    [hsx|
        <ul class="list-group">
            {forEach timecardEntries (renderTimecardEntry selectedPerson)}
        </ul>
    |]

renderTimecardEntry :: Person -> TimecardEntry -> Html
renderTimecardEntry selectedPerson timecardEntry =
    [hsx|
        <div class="card mb-4">
            <h5 class="card-header">
                {weekday'} - {date}
            </h5>

            <div class="card-body">
                <h5 class="card-title">{jobName}</h5>
                <p class="card-text">{invoiceTranslation}</p>
                <a href={editAction} class="btn btn-primary">Edit</a>
            </div>
        </div>
    |]
  where
    weekday' = weekday $ get #date timecardEntry
    date = TO.date (get #date timecardEntry)
    jobName = get #jobName timecardEntry
    invoiceTranslation = get #invoiceTranslation timecardEntry
    editAction = EditTimecardEntryAction (get #id selectedPerson) (get #id timecardEntry)

renderTimecardEntryForm :: Person -> [Message] -> TimecardActivity -> TimecardEntry -> Html
renderTimecardEntryForm selectedPerson selectedMessages timecardActivity timecardEntry =
    formForWithOptions
        timecardEntry
        formOptions
        [hsx|
            {(dateTimeField #date) { fieldClass = "date-time-field"}}
            {(textField #jobName)}
            {(textField #hoursWorked)}

            <div id="form-group-timecardEntry_workDone" class="form-group">
                <label for="timecardEntry_workDone">Work Done</label>
                <textarea id="timecardEntry_workDone" name="workDone" class="form-control">
                    {workDone}
                </textarea>
            </div>

            <div id="form-group-timecardEntry_invoiceTranslation" class="form-group">
                <label for="timecardEntry_invoiceTranslation">Invoice Translation</label>
                <textarea 
                    id="timecardEntry_invoiceTranslation"
                    name="invoiceTranslation"
                    class="form-control">
                    {invoiceTranslations}
                </textarea>
            </div>

            <input 
                type="hidden"
                name="selectedMessageIds"
                id="selectedMessageIds"
                class="form-control"
                value={selectedMessagesParam}
            />

            <input
                type="hidden"
                name="selectedPersonId"
                id="selectedPersonId"
                class="form-control"
                value={selectedPersonId}
            />

            {submitButton { label = submitLabel } }
            <a href={cancelAction} class="btn btn-secondary ml-2" role="button">Cancel</a>
        |]
  where
    formOptions formContext = formContext |> set #formId "timecard-form"
    workDone = renderMessageBodies (get #workDone timecardEntry) sortedMessages
    invoiceTranslations = renderMessageBodies (get #invoiceTranslation timecardEntry) sortedMessages
    selectedPersonId = show $ get #id selectedPerson
    submitLabel = if timecardActivity == CreatingEntry then "Create" else "Update"
    cancelAction = PersonSelectionAction (get #id selectedPerson)
    selectedMessagesParam = intercalate "," (show . get #id <$> sortedMessages)
    sortedMessages = sortBy (\m1 m2 -> get #createdAt m1 `compare` get #createdAt m2) selectedMessages

renderMessageBodies :: Text -> [Message] -> Html
renderMessageBodies existingText messages =
    if existingText == ""
        then [hsx| {intercalate "\n\n" ((get #body) <$> messages)} |]
        else [hsx| {existingText} |]

renderSentAt :: Message -> Html
renderSentAt message =
    let sentAt = get #createdAt message
     in [hsx|
            <time class="date-time" datetime={show sentAt}>
                {show sentAt}
            </time>
        |]

messageStatusClass :: MessageStatus -> Text
messageStatusClass status =
    case status of
        Delivered -> "message-status delivered"
        Received -> "message-status received"
        Failed -> "message-status failed"
        _ -> "message-status sending"

renderScheduledFor :: SendMessageAction.T -> Html
renderScheduledFor scheduledMessage =
    let runsAt = get #runsAt scheduledMessage
     in [hsx|
            <time class="date-time" datetime={show runsAt}>
                {show runsAt}
            </time>
        |]

sendMessageFormOptions :: FormContext TwilioMessage -> FormContext TwilioMessage
sendMessageFormOptions formContext =
    formContext
        |> set #formId "send-message-form"
        |> set #formAction (pathTo CreateOutgoingPhoneMessageAction)

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

        .message-scheduled-for {
            font-size: 80%;
            color: black;
        }

        .timecard-column {
            height: calc(100vh - 150px);
            overflow-y: scroll;
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
