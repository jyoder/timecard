module Web.View.TwilioMessages.Show where

import Web.View.Prelude

data ShowView = ShowView {twilioMessage :: TwilioMessage}

instance View ShowView where
    html ShowView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={TwilioMessagesAction}>TwilioMessages</a></li>
                <li class="breadcrumb-item active">Show TwilioMessage</li>
            </ol>
        </nav>
        <h1>Show TwilioMessage</h1>
        <p>{twilioMessage}</p>
    |]
