module Web.View.PhoneNumbers.Edit where
import Web.View.Prelude

data EditView = EditView { phoneNumber :: PhoneNumber }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PhoneNumbersAction}>Phone Numbers</a></li>
                <li class="breadcrumb-item active">Edit Phone Number</li>
            </ol>
        </nav>
        <h1>Edit Phone Number</h1>
        {renderForm phoneNumber}
    |]

renderForm :: PhoneNumber -> Html
renderForm phoneNumber = formFor phoneNumber [hsx|
    {(textField #number)}
    {submitButton { label = "Save Phone Number"}}
|]
