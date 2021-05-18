module Web.View.Communications.Index where

import Web.View.Communications.Communication
import Web.View.Prelude

data IndexView = IndexView
    { persons :: ![Person]
    , selectedPerson :: !Person
    , communications :: ![Communication]
    , newMessage :: !TwilioMessage
    }

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
                <div class="list-group">
                    {forEach communications (renderCommunication_)}
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
