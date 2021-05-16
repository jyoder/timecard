module Web.View.TwilioMessages.New where

import Web.View.Prelude

data NewView = NewView {twilioMessage :: TwilioMessage}

instance View NewView where
    html NewView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={TwilioMessagesAction}>TwilioMessages</a></li>
                <li class="breadcrumb-item active">New TwilioMessage</li>
            </ol>
        </nav>
        <h1>New TwilioMessage</h1>
        {renderForm twilioMessage}
    |]

renderForm :: TwilioMessage -> Html
renderForm twilioMessage =
    formFor
        twilioMessage
        [hsx|
    {(textField #phoneMessageId)}
    {(textField #apiVersion)}
    {(textField #messageSid)}
    {(textField #accountSid)}
    {(textField #messagingServiceSid)}
    {(textField #messageStatus)}
    {(textField #fromNumber)}
    {(textField #toNumber)}
    {(textField #body)}
    {(textField #numMedia)}
    {submitButton}
|]
