module Web.View.WorkerPreferences.Edit where
import Web.View.Prelude

data EditView = EditView { workerPreference :: WorkerPreference }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkerPreferencesAction}>WorkerPreferences</a></li>
                <li class="breadcrumb-item active">Edit WorkerPreference</li>
            </ol>
        </nav>
        <h1>Edit WorkerPreference</h1>
        {renderForm workerPreference}
    |]

renderForm :: WorkerPreference -> Html
renderForm workerPreference = formFor workerPreference [hsx|
    {(textField #personId)}
    {(textField #sendDailyReminderAt)}
    {submitButton}
|]
