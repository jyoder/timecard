module Web.View.PhoneNumbers.New where
import Web.View.Prelude

data NewView = NewView { phoneNumber :: PhoneNumber }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PhoneNumbersAction}>Phone Numbers</a></li>
                <li class="breadcrumb-item active">New Phone Number</li>
            </ol>
        </nav>
        <h1>New Phone Number</h1>
        {renderForm phoneNumber}
    |]

renderForm :: PhoneNumber -> Html
renderForm phoneNumber = formFor phoneNumber [hsx|
    {(textField #number)}
    {submitButton  { label = "Create Phone Number"}}
|]
