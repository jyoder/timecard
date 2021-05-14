module Web.View.PhoneContacts.Show where

import Web.View.Prelude

data ShowView = ShowView {phoneContact :: PhoneContact}

instance View ShowView where
    html ShowView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PhoneContactsAction}>Phone Contacts</a></li>
                <li class="breadcrumb-item active">Show Phone Contact</li>
            </ol>
        </nav>
        <h1>Show Phone Contact</h1>
        <p>{phoneContact}</p>
    |]
