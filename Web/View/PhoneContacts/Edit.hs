module Web.View.PhoneContacts.Edit where

import Web.View.Prelude

data EditView = EditView {phoneContact :: PhoneContact}

instance View EditView where
    html EditView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PhoneContactsAction}>Phone Contacts</a></li>
                <li class="breadcrumb-item active">Edit Phone Contact</li>
            </ol>
        </nav>
        <h1>Edit Phone Contact</h1>
        {renderForm phoneContact}
    |]

renderForm :: PhoneContact -> Html
renderForm phoneContact =
    formFor
        phoneContact
        [hsx|
    {(textField #personId)}
    {(textField #phoneNumberId)}
    {submitButton { label = "Save Phone Contact"}}
|]
