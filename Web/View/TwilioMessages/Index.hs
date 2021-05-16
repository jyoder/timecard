module Web.View.TwilioMessages.Index where

import Web.View.Prelude

data IndexView = IndexView {twilioMessages :: [TwilioMessage]}

instance View IndexView where
    html IndexView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={TwilioMessagesAction}>TwilioMessages</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewTwilioMessageAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>TwilioMessage</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach twilioMessages renderTwilioMessage}</tbody>
            </table>
        </div>
    |]

renderTwilioMessage twilioMessage =
    [hsx|
    <tr>
        <td>{twilioMessage}</td>
        <td><a href={ShowTwilioMessageAction (get #id twilioMessage)}>Show</a></td>
        <td><a href={EditTwilioMessageAction (get #id twilioMessage)} class="text-muted">Edit</a></td>
        <td><a href={DeleteTwilioMessageAction (get #id twilioMessage)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
