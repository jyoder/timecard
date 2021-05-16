module Web.View.TwilioMessages.Edit where

import Web.View.Prelude

data EditView = EditView {twilioMessage :: TwilioMessage}

instance View EditView where
    html EditView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={TwilioMessagesAction}>TwilioMessages</a></li>
                <li class="breadcrumb-item active">Edit TwilioMessage</li>
            </ol>
        </nav>
        <h1>Edit TwilioMessage</h1>
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
