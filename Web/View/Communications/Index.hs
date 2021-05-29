module Web.View.Communications.Index where

import Data.Time.Format.ISO8601 (iso8601Show)
import IHP.View.TimeAgo as TO
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.View.Prelude

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

instance View IndexView where
    html view =
        [hsx|
            <nav class="navbar navbar-light bg-light">
                <div class="container-fluid">
                    <span class="navbar-brand mb-0 h1">Timecard Communication</span>
                    <a href={DeleteSessionAction} class="btn btn-outline-primary js-delete js-delete-no-confirm">Logout</a>
                </div>
            </nav>

            <div class="row align-items start">
                {renderPeopleColumn view}
                {renderMessagesColumn view}
                <div class="timecard col-4">
                    {renderTimecardColumn view}
                </div>
            </div>

            {styles}
        |]

renderPeopleColumn :: IndexView -> Html
renderPeopleColumn IndexView {..} =
    [hsx|
        <div class="person-selector col-2">
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
                    {renderMessages selectedPerson personActivity selectedMessageIds messages}
                    <div class="communications-composer">
                        {renderSendMessageForm toPhoneNumber newMessage}
                    </div>
                </div>
            |]

renderMessages :: Person -> PersonActivity -> [Id TwilioMessage] -> [Message] -> Html
renderMessages selectedPerson personActivity selectedMessageIds messages =
    [hsx|
        <div class="communications-history list-group-flush">
            {forEach messages (renderMessage selectedPerson personActivity selectedMessageIds)}
            <div class="scroll-pinned"></div>
        </div>
    |]

renderMessage :: Person -> PersonActivity -> [Id TwilioMessage] -> Message -> Html
renderMessage selectedPerson personActivity selectedMessageIds message =
    [hsx|
        <div
            class="list-group-item flex-column align-items-start">
            <div class="d-flex w-100 justify-content-between">
                <h5 class="mb-1">{get #fromName message}</h5>
                <small>{renderSentAt message}</small>
            </div>
            <p class="communication-body mb-1">{get #body message}</p>

            <div class="d-flex w-100 justify-content-between">
                <small class={messageStatusClass $ get #status message}>
                    {show $ get #status message}
                </small>
                <a href={nextAction}
                data-turbolinks="false"
                class={"btn btn-outline-primary btn-sm " <> activeClass'}>
                    {linkButtonText}
                </a>
            </div>
        </div>
    |]
  where
    selectedPersonId = get #id selectedPerson
    isSelected = messageId `elem` selectedMessageIds
    toggledMessageIds = show <$> if isSelected then excludeMessageId else includeMessageId
    excludeMessageId = filter (/= messageId) selectedMessageIds
    includeMessageId = messageId : selectedMessageIds
    messageId = get #id message
    activeClass' = activeClass isSelected
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

renderTimecardColumn :: IndexView -> Html
renderTimecardColumn IndexView {..} =
    case personSelection of
        NoPersonSelected -> [hsx|<p>barf</p>|]
        PersonSelected {..} ->
            case personActivity of
                SendingMessage {..} -> renderTimecardEntries selectedPerson timecardEntries
                WorkingOnTimecardEntry {..} -> renderTimecardEntryForm selectedPerson selectedMessages timecardActivity timecardEntry

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
            <h5 class="card-header">{weekday (get #date timecardEntry)} - {TO.date (get #date timecardEntry)}</h5>

            <div class="card-body">
                <h5 class="card-title">{get #jobName timecardEntry}</h5>
                <p class="card-text">{get #invoiceTranslation timecardEntry}</p>
                <a href={EditTimecardEntryAction (get #id selectedPerson) (get #id timecardEntry)} class="btn btn-primary">Edit</a>
            </div>
        </div>
    |]

weekday :: UTCTime -> Html
weekday = timeElement "weekday"

timeElement :: Text -> UTCTime -> Html
timeElement className dateTime = H.time ! A.class_ (cs className) ! A.datetime (cs $ iso8601Show dateTime) $ cs (beautifyUtcTime dateTime)

beautifyUtcTime :: UTCTime -> String
beautifyUtcTime = formatTime defaultTimeLocale "%d.%m.%Y, %H:%M"

renderTimecardEntryForm :: Person -> [Message] -> TimecardActivity -> TimecardEntry -> Html
renderTimecardEntryForm selectedPerson selectedMessages timecardActivity timecardEntry =
    formForWithOptions
        timecardEntry
        timecardEntryFormOptions
        [hsx|
            {(dateTimeField #date) { fieldClass = "date-time-field"}}
            {(textField #jobName)}
            {(textField #hoursWorked)}

            <div id="form-group-timecardEntry_workDone" class="form-group">
                <label for="timecardEntry_workDone">Work Done</label>
                <textarea id="timecardEntry_workDone" name="workDone" class="form-control">
                    {renderMessageBodies (get #workDone timecardEntry) sortedMessages}
                </textarea>
            </div>

            <div id="form-group-timecardEntry_invoiceTranslation" class="form-group">
                <label for="timecardEntry_invoiceTranslation">Invoice Translation</label>
                <textarea id="timecardEntry_invoiceTranslation" name="invoiceTranslation" class="form-control">
                    {renderMessageBodies (get #invoiceTranslation timecardEntry) sortedMessages}
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
                value={show $ get #id selectedPerson}
            />

            {submitButton { label = submitLabel } }
            <a href={cancelAction} class="btn btn-secondary ml-2" role="button">Cancel</a>
        |]
  where
    sortedMessages = sortBy (\m1 m2 -> get #createdAt m1 `compare` get #createdAt m2) selectedMessages
    submitLabel = if timecardActivity == CreatingEntry then "Create" else "Update"
    selectedMessagesParam = intercalate "," (show . get #id <$> sortedMessages)
    cancelAction = PersonSelectionAction (get #id selectedPerson)

renderMessageBodies :: Text -> [Message] -> Html
renderMessageBodies existingText messages =
    if existingText == ""
        then forEach messages renderMessageBody
        else [hsx| {existingText} |]

renderMessageBody :: Message -> Html
renderMessageBody message = [hsx| {get #body message <> "\n\n"} |]

renderSendMessageForm :: PhoneNumber -> TwilioMessage -> Html
renderSendMessageForm phoneNumber newMessage =
    [hsx|
        <div class="communications-composer">
            <form method="POST" action="/CreateOutgoingPhoneMessage" id="send-message-form" class="new-form" data-disable-javascript-submission="false">
                <div class="form-group" id="form-group-twilioMessage_toId">
                    <input type="hidden" name="toId" placeholder="" id="twilioMessage_toId" class="form-control" value={show $ get #id phoneNumber}>
                </div>
                <div class="input-group">
                    <textarea class="form-control" id="twilioMessage_body" name="body" rows="2"></textarea>
                    <div class="input-group-append">
                        <button class="btn btn-primary">Send</button>
                    </div>
                </div>
            </form>
        </div>
    |]

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
        _ -> "message-status sending" -- TODO: make "in-progress" instead of sending

activeClass :: Bool -> Text
activeClass isSelected = if isSelected then "active" else ""

sendMessageFormOptions :: FormContext TwilioMessage -> FormContext TwilioMessage
sendMessageFormOptions formContext =
    formContext
        |> set #formId "send-message-form"
        |> set #formAction (pathTo CreateOutgoingPhoneMessageAction)

timecardEntryFormOptions :: FormContext TimecardEntry -> FormContext TimecardEntry
timecardEntryFormOptions formContext =
    formContext
        |> set #formId "timecard-form"

styles :: Html
styles =
    [hsx|
    <style>
        .message-status.delivered {
            font-size: 80%;
            color: green;
        }

        .message-status.received {
            font-size: 80%;
            color: rgb(26, 124, 236);
        }

        .message-status.failed {
            font-size: 80%;
            color: red;
        }

        .message-status.sending {
            font-size: 80%;
            color: darkgray;
        }

        .person-selector {
            height: calc(100vh - 150px);
            overflow-y: scroll;
        }

        .communications-history {
            height: calc(100vh - 270px);
            overflow-y: scroll;
        }

        .communications-composer {
            height: 100px;
            padding: 0px;
            margin: 0px;
        }

        .communication-body {
            white-space: pre-line;
        }

        .timecard {
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

        /* Remove the scrollbar from Chrome, Safari, Edge and IEw */
        ::-webkit-scrollbar {
            width: 0px;
            background: transparent;
        }

        * {
        -ms-overflow-style: none !important;
        }
    </style>
|]
