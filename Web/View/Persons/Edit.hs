module Web.View.Persons.Edit where

import Web.View.Prelude

data EditView = EditView {person :: Person}

instance View EditView where
    html EditView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PersonsAction}>Persons</a></li>
                <li class="breadcrumb-item active">Edit Person</li>
            </ol>
        </nav>
        <h1>Edit Person</h1>
        {renderForm person}
    |]

renderForm :: Person -> Html
renderForm person =
    formFor
        person
        [hsx|
    {textField #firstName}
    {textField #lastName}
    {textField #goesBy}
    {submitButton}
|]
