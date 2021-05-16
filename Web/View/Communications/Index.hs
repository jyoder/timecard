module Web.View.Communications.Index where

import Web.View.Prelude

data IndexView = IndexView
    { persons :: ![Person]
    , selectedPerson :: !Person
    , communications :: ![Communication]
    , newMessage :: !TwilioMessage
    }

data Communication = Communication
    { messageId :: UUID
    , nameA :: Text
    , nameB :: Text
    , isFromPersonA :: Bool
    , createdAt :: UTCTime
    , wasDelivered :: Bool
    , messageBody :: Text
    }
    deriving (Show)

instance View IndexView where
    html IndexView {..} =
        [hsx|
        <nav class="navbar navbar-light bg-light mb-5">
            <div class="container-fluid">
                <span class="navbar-brand mb-0 h1">Timecard Communication Center</span>
            </div>
        </nav>
        <div class="row align-items start">
            <div class="col-3 pr-5">
                <div class="list-group">
                    {forEach persons (renderPerson selectedPerson)}
                </div>
            </div>
            <div class="col-6">
                <div>
                    {forEach communications renderCommunication}
                </div>
                <div>
                    {renderSendMessageForm newMessage}
                </div>
            </div>
        </div>
    |]

renderPerson :: Person -> Person -> Html
renderPerson selectedPerson person =
    if get #id person == get #id selectedPerson
        then renderSelectedPerson person
        else renderNonSelectedPerson person

renderNonSelectedPerson :: Person -> Html
renderNonSelectedPerson person =
    [hsx|
    <a href={CommunicationsAction $ get #id person} class="list-group-item">
        {get #firstName person} {get #lastName person}
    </a>
|]

renderSelectedPerson :: Person -> Html
renderSelectedPerson person =
    [hsx|
    <a href={CommunicationsAction $ get #id person} class="list-group-item active" aria-current="true">
        {get #firstName person} {get #lastName person}
    </a>
|]

renderCommunication :: Communication -> Html
renderCommunication communication =
    [hsx|
    <div class="mb-4">
        <div class="pb-1">
            <span class="message--sender pr-2">{senderName communication}</span>
            {renderSentAt communication}
            <span class="message--delivery-status">{deliveryStatus communication}</span>
        </div>
        <div class="message--body ">
            {get #messageBody communication}
        </div>
    </div>
|]

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
            <time class="message--sent-at date-time pr-2" datetime={show sentAt}>
                {show sentAt}
            </time>
        |]

deliveryStatus :: Communication -> Text
deliveryStatus communication =
    if get #wasDelivered communication
        then "delivered"
        else "sending"

senderName :: Communication -> Text
senderName communication =
    if get #isFromPersonA communication
        then get #nameA communication
        else get #nameB communication
