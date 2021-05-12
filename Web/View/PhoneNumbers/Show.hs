module Web.View.PhoneNumbers.Show where
import Web.View.Prelude

data ShowView = ShowView { phoneNumber :: PhoneNumber }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PhoneNumbersAction}>PhoneNumbers</a></li>
                <li class="breadcrumb-item active">Show PhoneNumber</li>
            </ol>
        </nav>
        <h1>Show PhoneNumber</h1>
        <p>{phoneNumber}</p>
    |]
