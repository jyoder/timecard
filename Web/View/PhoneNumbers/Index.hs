module Web.View.PhoneNumbers.Index where

import Web.View.Prelude

data IndexView = IndexView {phoneNumbers :: [PhoneNumber]}

instance View IndexView where
    html IndexView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={PhoneNumbersAction}>Phone Numbers</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewPhoneNumberAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Phone Number</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach phoneNumbers renderPhoneNumber}</tbody>
            </table>
        </div>
    |]

renderPhoneNumber phoneNumber =
    [hsx|
    <tr>
        <td>{phoneNumber}</td>
        <td><a href={ShowPhoneNumberAction (get #id phoneNumber)}>Show</a></td>
        <td><a href={EditPhoneNumberAction (get #id phoneNumber)} class="text-muted">Edit</a></td>
        <td><a href={DeletePhoneNumberAction (get #id phoneNumber)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
