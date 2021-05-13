module Web.View.PhoneMessages.New where
import Web.View.Prelude

data NewView = NewView { phoneMessage :: PhoneMessage }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PhoneMessagesAction}>Phone Messages</a></li>
                <li class="breadcrumb-item active">New Phone Message</li>
            </ol>
        </nav>
        <h1>New Phone Message</h1>
        {renderForm phoneMessage}
    |]

renderForm :: PhoneMessage -> Html
renderForm phoneMessage = formFor phoneMessage [hsx|
    {(textField #toId)}
    {(textField #fromId)}
    {(dateTimeField #sentAt)}
    {(textField #body)}
    {submitButton { label = "Create Phone Message"} }
|]
