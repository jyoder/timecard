module Web.View.PhoneMessages.Edit where

import Web.View.Prelude

data EditView = EditView {phoneMessage :: PhoneMessage}

instance View EditView where
    html EditView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PhoneMessagesAction}>Phone Messages</a></li>
                <li class="breadcrumb-item active">Edit Phone Message</li>
            </ol>
        </nav>
        <h1>Edit Phone Message</h1>
        {renderForm phoneMessage}
    |]

renderForm :: PhoneMessage -> Html
renderForm phoneMessage =
    formFor
        phoneMessage
        [hsx|
    {(textField #toId)}
    {(textField #fromId)}
    {(dateTimeField #sentAt)}
    {(textField #body)}
    {submitButton { label = "Save Phone Message"} }
|]
