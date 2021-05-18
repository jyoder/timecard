module Web.View.Communications.Communication where

import Web.Controller.Prelude
import Web.View.Prelude

instance View CommunicationView where
    beforeRender view = setLayout (\view -> view)

    html CommunicationView {..} = renderCommunication isSelected communication

data CommunicationView = CommunicationView
    { isSelected :: !Bool
    , communication :: !Communication
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
    deriving (Show)

renderCommunication_ :: Communication -> Html
renderCommunication_ = renderCommunication False

renderCommunication :: Bool -> Communication -> Html
renderCommunication isSelected communication =
    [hsx|
    <a 
        hx-get={ClickCommunicationAction (get #messageId communication) (show $ not isSelected)}
        hx-swap="outerHTML"
        class={"list-group-item list-group-item-action flex-column align-items-start border-0 " <> activeClass isSelected}>
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
            <time class="date-time pr-2" datetime={show sentAt}>
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

activeClass :: Bool -> Text
activeClass isSelected = if isSelected then "active" else ""