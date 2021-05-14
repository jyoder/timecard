module Web.View.PhoneContacts.Index where

import Web.View.Prelude

data IndexView = IndexView {phoneContacts :: [PhoneContact]}

instance View IndexView where
    html IndexView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={PhoneContactsAction}>Phone Contacts</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewPhoneContactAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Phone Contact</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach phoneContacts renderPhoneContact}</tbody>
            </table>
        </div>
    |]

renderPhoneContact phoneContact =
    [hsx|
    <tr>
        <td>{phoneContact}</td>
        <td><a href={ShowPhoneContactAction (get #id phoneContact)}>Show</a></td>
        <td><a href={EditPhoneContactAction (get #id phoneContact)} class="text-muted">Edit</a></td>
        <td><a href={DeletePhoneContactAction (get #id phoneContact)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
