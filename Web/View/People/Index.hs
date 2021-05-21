module Web.View.People.Index where

import Web.View.Prelude

data IndexView = IndexView {people :: [Person]}

instance View IndexView where
    html IndexView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={PeopleAction}>People</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewPersonAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Person</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach people renderPerson}</tbody>
            </table>
        </div>
    |]

renderPerson person =
    [hsx|
    <tr>
        <td>{person}</td>
        <td><a href={ShowPersonAction (get #id person)}>Show</a></td>
        <td><a href={EditPersonAction (get #id person)} class="text-muted">Edit</a></td>
        <td><a href={DeletePersonAction (get #id person)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
