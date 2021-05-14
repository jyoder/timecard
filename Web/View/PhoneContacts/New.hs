module Web.View.PhoneContacts.New where

import Web.View.Prelude

data NewView = NewView {phoneContact :: PhoneContact}

instance View NewView where
    html NewView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PhoneContactsAction}>Phone Contacts</a></li>
                <li class="breadcrumb-item active">New Phone Contact</li>
            </ol>
        </nav>
        <h1>New Phone Contact</h1>
        {renderForm phoneContact}
    |]

renderForm :: PhoneContact -> Html
renderForm phoneContact =
    formFor
        phoneContact
        [hsx|
    {(textField #personId)}
    {(textField #phoneNumberId)}
    {submitButton { label = "Create Phone Contact"}}
|]
