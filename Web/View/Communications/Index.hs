module Web.View.Communications.Index where

import IHP.View.TimeAgo as TO
import Web.View.Prelude

data IndexView = IndexView
    { persons :: ![Person]
    , selectedPerson :: !(Maybe Person)
    , communications :: ![Communication]
    , selectedCommunications :: ![Communication]
    , timecardEntries :: ![TimecardEntry]
    , newMessage :: !(Maybe TwilioMessage)
    , newTimecardEntry :: !(Maybe TimecardEntry)
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
            </div>
        </nav>
        <div class="row align-items start">
            {renderPersons view}
            {renderCommunications view}
            <div class="timecard col-4">
                {renderTimecard view}
            </div>
        </div>
        {styles}
    |]

renderPersons :: IndexView -> Html
renderPersons IndexView {..} =
    [hsx|
    <div class="person-selector col-2">
        <div class="list-group">
            {forEach persons (renderPerson selectedPerson)}
        </div>
    </div>
|]

renderCommunications :: IndexView -> Html
renderCommunications IndexView {..} =
    case (selectedPerson, newMessage) of
        (Just selectedPerson, Just newMessage) ->
            [hsx|
            <div class="col-6">
                <div class="communications-history list-group">
                    {forEach communications (renderCommunication selectedPerson selectedCommunications)}
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
    <a href="#" class="btn btn-primary">Edit</a>
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
                
                {(dateTimeField #date)}
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

                {submitButton { label = "Create" } }
            |]
        Nothing -> [hsx||]
  where
    sortedCommunications = sortBy (\a b -> get #createdAt a `compare` get #createdAt b) selectedCommunications

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

renderCommunication :: Person -> [Communication] -> Communication -> Html
renderCommunication selectedPerson selectedCommunications communication =
    [hsx|
    <a
        hx-get={CommunicationsForAction selectedPersonId toggledCommunicationIds}
        hx-target="body"
        class={"list-group-item list-group-item-action flex-column align-items-start border-0 " <> active}>
        <div class="d-flex w-100 justify-content-between">
            <h5 class="mb-1">{senderName communication}</h5>
            <small>{renderSentAt communication}</small>
        </div>
        <p class="communication-body mb-1">{get #messageBody communication}</p>
        <small class={deliveryStatusClass communication}>
            {get #status communication}
        </small>
    </a>
|]
  where
    selectedPersonId = get #id selectedPerson
    isSelected = isCommunicationSelected selectedCommunications communication
    toggledCommunicationIds = communicationIds $ toggleCommunicationSelected selectedCommunications communication
    active = activeClass isSelected

renderSendMessageForm :: TwilioMessage -> Html
renderSendMessageForm phoneMessage =
    formForWithOptions
        phoneMessage
        sendMessageFormOptions
        [hsx|
        {(hiddenField #toId)}
        {(textField #body) { fieldLabel = "" }}
        {submitButton { label = "Send" } }
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
