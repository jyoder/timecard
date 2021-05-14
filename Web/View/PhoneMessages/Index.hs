module Web.View.PhoneMessages.Index where

import Web.View.Prelude

data IndexView = IndexView {phoneMessages :: [PhoneMessage]}

instance View IndexView where
    html IndexView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={PhoneMessagesAction}>Phone Messages</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewPhoneMessageAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Phone Message</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach phoneMessages renderPhoneMessage}</tbody>
            </table>
        </div>
    |]

renderPhoneMessage phoneMessage =
    [hsx|
    <tr>
        <td>{phoneMessage}</td>
        <td><a href={ShowPhoneMessageAction (get #id phoneMessage)}>Show</a></td>
        <td><a href={EditPhoneMessageAction (get #id phoneMessage)} class="text-muted">Edit</a></td>
        <td><a href={DeletePhoneMessageAction (get #id phoneMessage)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
