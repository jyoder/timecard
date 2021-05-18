module Web.View.Communications.Index where

import Web.View.Communications.Communication
import Web.View.Prelude

data IndexView = IndexView
    { persons :: ![Person]
    , selectedPerson :: !(Maybe Person)
    , communications :: ![Communication]
    , newMessage :: !(Maybe TwilioMessage)
    }

instance View IndexView where
    html view =
        [hsx|
        <nav class="navbar navbar-light bg-light mb-5">
            <div class="container-fluid">
                <span class="navbar-brand mb-0 h1">Timecard Communication Center</span>
            </div>
        </nav>
        <div class="row align-items start">
            {renderPersons view}
            {renderCommunications view}
        </div>
    |]

renderPersons :: IndexView -> Html
renderPersons IndexView {..} =
    [hsx|
    <div class="col-3 pr-5">
        <div class="list-group">
            {forEach persons (renderPerson selectedPerson)}
        </div>
    </div>
|]

renderCommunications :: IndexView -> Html
renderCommunications IndexView {..} =
    case newMessage of
        Just newMessage ->
            [hsx|
            <div class="col-6">
                <div class="list-group">
                    {forEach communications (renderCommunication_)}
                </div>
                <div>
                    {renderSendMessageForm newMessage}
                </div>
            </div>
        |]
        Nothing -> [hsx||]

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
    <a href={CommunicationsForAction $ get #id person} class="list-group-item">
        {get #firstName person} {get #lastName person}
    </a>
|]

renderSelectedPerson :: Person -> Html
renderSelectedPerson person =
    [hsx|
    <a href={CommunicationsForAction $ get #id person} class="list-group-item active" aria-current="true">
        {get #firstName person} {get #lastName person}
    </a>
|]
