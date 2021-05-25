module Web.View.Communications.Index where

import IHP.View.TimeAgo as TO
import Web.View.Prelude

data IndexView = IndexView
    { people :: ![Person]
    , selectedPerson :: !(Maybe Person)
    , communications :: ![Communication]
    , selectedCommunications :: ![Communication]
    , timecardEntries :: ![TimecardEntry]
    , newMessage :: !(Maybe TwilioMessage)
    , newTimecardEntry :: !(Maybe TimecardEntry)
    , editingTimecard :: Bool
    }

data Communication = Communication
    { messageId :: !(Id TwilioMessage)
    , nameA :: !Text
    , nameB :: !Text
    , isFromPersonA :: !Bool
    , createdAt :: !UTCTime
    , status :: !Text
    , messageBody :: !Text
    }
    deriving (Show, Eq)

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
            {renderPeople view}
            {renderCommunications view}
            <div class="timecard col-4">
                {renderTimecard view}
            </div>
        </div>
        {styles}
    |]

renderPeople :: IndexView -> Html
renderPeople IndexView {..} =
    [hsx|
    <div class="person-selector col-2">
        <div class="list-group">
            {forEach people (renderPerson selectedPerson)}
        </div>
    </div>
|]

--
renderCommunications :: IndexView -> Html
renderCommunications IndexView {..} =
    case (selectedPerson, newMessage) of
        (Just selectedPerson, Just newMessage) ->
            [hsx|
            <div class="col-6">
                <div class="communications-history list-group-flush">
                    {forEach communications (renderCommunication selectedPerson selectedCommunications newTimecardEntry editingTimecard)}
                    <div class="scroll-pinned"></div>
                </div>
                <div class="communications-composer">
                    {renderSendMessageForm newMessage}
                </div>
            </div>
            |]
        _ -> [hsx||]

renderTimecard :: IndexView -> Html
renderTimecard view =
    if null $ selectedCommunications view
        then renderTimecardEntries view
        else renderTimecardForm view

renderTimecardEntries :: IndexView -> Html
renderTimecardEntries IndexView {..} =
    [hsx|
        <ul class="list-group">
            {forEach timecardEntries renderTimecardEntry}
        </ul>
    |]

renderTimecardEntry :: TimecardEntry -> Html
renderTimecardEntry timecardEntry =
    [hsx|
<div class="card mb-4">
  <h5 class="card-header">{TO.date (get #date timecardEntry)}</h5>
  <div class="card-body">
    <h5 class="card-title">{get #jobName timecardEntry}</h5>
    <p class="card-text">{get #invoiceTranslation timecardEntry}</p>
    <a href={EditTimecardEntry (get #id timecardEntry) [] "True"} class="btn btn-primary">Edit</a>
  </div>
</div>
    |]

renderTimecardForm :: IndexView -> Html
renderTimecardForm IndexView {..} =
    case newTimecardEntry of
        Just newTimecardEntry ->
            formForWithOptions
                newTimecardEntry
                timecardEntryFormOptions
                [hsx|
                {(dateTimeField #date) { fieldClass = "date-time-field"}}
                {(textField #jobName)}
                {(numberField #hoursWorked)}

                <div id="form-group-timecardEntry_workDone" class="form-group">
                    <label for="timecardEntry_workDone">Work Done</label>
                    <textarea id="timecardEntry_workDone" name="workDone" class="form-control">
                        {textareaContent (get #workDone newTimecardEntry) sortedCommunications}
                    </textarea>
                </div>

                <div id="form-group-timecardEntry_invoiceTranslation" class="form-group">
                    <label for="timecardEntry_invoiceTranslation">Invoice Translation</label>
                    <textarea id="timecardEntry_invoiceTranslation" name="invoiceTranslation" class="form-control">
                        {textareaContent (get #invoiceTranslation newTimecardEntry) sortedCommunications}
                    </textarea>
                </div>

                <input 
                    type="hidden"
                    name="linkedMessageIds"
                    id="linkedMessageIds"
                    class="form-control"
                    value={linkedMessages sortedCommunications}
                />

                <input
                    type="hidden"
                    name="selectedPersonId"
                    id="selectedPersonId"
                    class="form-control"
                    value={fromMaybe "" (show . (get #id) <$> selectedPerson)}
                />

                {submitButton { label = submitLabel } }
                <a href={cancelAction} class="btn btn-secondary ml-2" role="button">Cancel</a>
            |]
        Nothing -> [hsx||]
  where
    sortedCommunications = sortBy (\a b -> get #createdAt a `compare` get #createdAt b) selectedCommunications
    submitLabel = if editingTimecard then "Update" else "Create"
    cancelAction = case selectedPerson of
        Just selectedPerson -> CommunicationsForAction (get #id selectedPerson) []
        Nothing -> CommunicationsAction

textareaContent :: Text -> [Communication] -> Html
textareaContent text communications =
    if text == ""
        then forEach communications tcText
        else [hsx| {text} |]

tcText :: Communication -> Html
tcText c = [hsx| {get #messageBody c <> "\n\n"} |]

linkedMessages :: [Communication] -> Text
linkedMessages communications =
    intercalate "," (show . get #messageId <$> communications)

renderPerson :: Maybe Person -> Person -> Html
renderPerson selectedPerson person =
    case selectedPerson of
        Just selectedPerson ->
            if get #id person == get #id selectedPerson
                then renderSelectedPerson person
                else renderNonSelectedPerson person
        Nothing -> renderNonSelectedPerson person

renderNonSelectedPerson :: Person -> Html
renderNonSelectedPerson person =
    [hsx|
    <a href={CommunicationsForAction (get #id person) []} class="list-group-item">
        {get #firstName person} {get #lastName person}
    </a>
|]

renderSelectedPerson :: Person -> Html
renderSelectedPerson person =
    [hsx|
    <a href={CommunicationsForAction (get #id person) []} class="list-group-item active" aria-current="true">
        {get #firstName person} {get #lastName person}
    </a>
|]

renderCommunication :: Person -> [Communication] -> Maybe TimecardEntry -> Bool -> Communication -> Html
renderCommunication selectedPerson selectedCommunications timecardEntry editingTimecard communication =
    [hsx|
    <div
        class="list-group-item flex-column align-items-start">
        <div class="d-flex w-100 justify-content-between">
            <h5 class="mb-1">{senderName communication}</h5>
            <small>{renderSentAt communication}</small>
        </div>
        <p class="communication-body mb-1">{get #messageBody communication}</p>

        <div class="d-flex w-100 justify-content-between">
            <small class={deliveryStatusClass communication}>
                {get #status communication}
            </small>
            <a href={nextAction}
               data-turbolinks="false"
               class={"btn btn-outline-primary btn-sm " <> active}>
                {activeText}
            </a>
        </div>
    </div>
|]
  where
    selectedPersonId = get #id selectedPerson
    isSelected = isCommunicationSelected selectedCommunications communication
    toggledCommunicationIds = communicationIds $ toggleCommunicationSelected selectedCommunications communication
    active = activeClass isSelected
    activeText = if isSelected then "Unlink" else "Link" :: Text
    nextAction = case (editingTimecard, timecardEntry) of
        (True, Just timecardEntry) -> EditTimecardEntry (get #id timecardEntry) toggledCommunicationIds "False"
        (_, _) -> CommunicationsForAction selectedPersonId toggledCommunicationIds

renderSendMessageForm :: TwilioMessage -> Html
renderSendMessageForm phoneMessage =
    [hsx|
        <form method="POST" action="/CreateOutgoingPhoneMessage" id="send-message-form" class="new-form" data-disable-javascript-submission="false">
            <div class="form-group" id="form-group-twilioMessage_toId">
                <input type="hidden" name="toId" placeholder="" id="twilioMessage_toId" class="form-control" value={show $ get #toId phoneMessage}>
            </div>
            <div class="input-group">
                <textarea class="form-control" id="twilioMessage_body" name="body" rows="2"></textarea>
                <div class="input-group-append">
                    <button class="btn btn-primary">Send</button>
                </div>
            </div>
        </form>
    |]

renderSentAt :: Communication -> Html
renderSentAt communication =
    let sentAt = get #createdAt communication
     in [hsx|
            <time class="date-time" datetime={show sentAt}>
                {show sentAt}
            </time>
        |]

deliveryStatusClass :: Communication -> Text
deliveryStatusClass communication =
    case get #status communication of
        "delivered" -> "message-status delivered"
        "received" -> "message-status received"
        "failed" -> "message-status failed"
        _ -> "message-status sending"

senderName :: Communication -> Text
senderName communication =
    if get #isFromPersonA communication
        then get #nameA communication
        else get #nameB communication

isCommunicationSelected :: [Communication] -> Communication -> Bool
isCommunicationSelected selected communication =
    get #messageId communication `elem` (get #messageId <$> selected)

toggleCommunicationSelected :: [Communication] -> Communication -> [Communication]
toggleCommunicationSelected selected communication =
    if isCommunicationSelected selected communication
        then filter (/= communication) selected
        else communication : selected

communicationIds :: [Communication] -> [Text]
communicationIds communications =
    show . get #messageId <$> communications

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
