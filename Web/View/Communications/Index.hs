module Web.View.Communications.Index where
import Web.View.Prelude

data IndexView = IndexView { 
    persons :: ![Person], 
    communications :: ![Communication],
    newMessage :: !PhoneMessage
}

data Communication = Communication { 
      nameA :: Text
    , nameB :: Text
    , isFromPersonA :: Bool
    , createdAt :: UTCTime
    , sentAt :: Maybe UTCTime
    , messageBody :: Text
} deriving Show

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="d-flex flex-column p-3" style="width: 250px;">
            <div class="d-flex align-items-center mb-3 mb-md-0 me-md-auto">
                <div>
                    {forEach persons renderPerson}
                </div>
            </div>
        </div>
        <div>
            <div>
                {forEach communications renderCommunication}
            </div>
        </div>
        <div>
            {renderSendMessageForm newMessage}
        </div>
    |]

renderPerson person = [hsx|
    <div>
        <a href={CommunicationsAction $ get #id person}>
            {get #firstName person} {get #lastName person}
        </a>
    </div>
|]

renderCommunication communication = [hsx|
    <div>
        <div>
            [{senderName communication} - {get #sentAt communication}]
        </div>
        <div>
            {get #messageBody communication}
        </div>
    </div>
|]

renderSendMessageForm :: PhoneMessage -> Html
renderSendMessageForm phoneMessage = 
    formFor' phoneMessage (pathTo CommunicationsCreateMessageAction) [hsx|
        {(hiddenField #toId)}
        {(textField #body)}
        {submitButton { label = "Send" } }
    |]

senderName :: Communication -> Text
senderName communication = 
    if get #isFromPersonA communication
        then get #nameA communication
        else get #nameB communication
