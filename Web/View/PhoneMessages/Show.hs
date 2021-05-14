module Web.View.PhoneMessages.Show where

import Web.View.Prelude

data ShowView = ShowView {phoneMessage :: PhoneMessage}

instance View ShowView where
    html ShowView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PhoneMessagesAction}>Phone Messages</a></li>
                <li class="breadcrumb-item active">Show Phone Message</li>
            </ol>
        </nav>
        <h1>Show Phone Message</h1>
        <p>{phoneMessage}</p>
    |]
