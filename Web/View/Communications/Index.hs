module Web.View.Communications.Index where

import Web.View.Prelude

data IndexView = IndexView
    { persons :: ![Person]
    , selectedPerson :: !(Maybe Person)
    , communications :: ![Communication]
    , selectedCommunicationIds :: ![Id TwilioMessage]
    , newMessage :: !(Maybe TwilioMessage)
    }

data Communication = Communication
    { messageId :: !(Id TwilioMessage)
    , nameA :: Text
    , nameB :: Text
    , isFromPersonA :: Bool
    , createdAt :: UTCTime
    , status :: Text
    , messageBody :: Text
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
            {renderTimecard view}
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
                    {forEach communications (renderCommunication selectedPerson selectedCommunicationIds)}
                    <div class="scroll-pinned"></div>
                </div>
                <div class="communications-composer">
                    {renderSendMessageForm newMessage}
                </div>
            </div>
            |]
        _ -> [hsx||]

renderTimecard :: IndexView -> Html
renderTimecard IndexView {..} =
    [hsx|
    <div class="timecard col-3">
        <p>Timecard</p>
    </div>
|]

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

renderCommunication :: Person -> [Id TwilioMessage] -> Communication -> Html
renderCommunication selectedPerson selectedCommunicationIds communication =
    [hsx|
    <a
        hx-get={CommunicationsForAction selectedPersonId toggledCommunicationIds}
        hx-target="body"
        class={"list-group-item list-group-item-action flex-column align-items-start border-0 " <> active}>
        <div class="d-flex w-100 justify-content-between">
            <h5 class="mb-1">{senderName communication}</h5>
            <small>{renderSentAt communication}</small>
        </div>
        <p class="mb-1">{get #messageBody communication}</p>
        <small class={deliveryStatusClass communication}>
            {get #status communication}
        </small>
    </a>
|]
  where
    selectedPersonId = get #id selectedPerson
    isSelected = isCommunicationSelected selectedCommunicationIds communication
    toggledCommunicationIds = communicationIds $ toggleCommunicationSelected selectedCommunicationIds communication
    active = activeClass isSelected

renderSendMessageForm :: TwilioMessage -> Html
renderSendMessageForm phoneMessage =
    formFor'
        phoneMessage
        (pathTo CreateOutgoingPhoneMessageAction)
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

isCommunicationSelected :: [Id TwilioMessage] -> Communication -> Bool
isCommunicationSelected selected communication =
    get #messageId communication `elem` selected

toggleCommunicationSelected :: [Id TwilioMessage] -> Communication -> [Id TwilioMessage]
toggleCommunicationSelected selected communication =
    if isCommunicationSelected selected communication
        then filter (\s -> s /= get #messageId communication) selected
        else get #messageId communication : selected

communicationIds :: [Id TwilioMessage] -> [Text]
communicationIds ids = show <$> ids

activeClass :: Bool -> Text
activeClass isSelected = if isSelected then "active" else ""

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
