module Web.View.Persons.New where

import Web.View.Prelude

data NewView = NewView {person :: Person}

instance View NewView where
    html NewView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PersonsAction}>Persons</a></li>
                <li class="breadcrumb-item active">New Person</li>
            </ol>
        </nav>
        <h1>New Person</h1>
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
