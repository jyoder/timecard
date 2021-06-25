module Web.View.WorkerPreferences.New where

import Web.View.Prelude

data NewView = NewView {workerPreference :: WorkerPreference}

instance View NewView where
    html NewView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkerPreferencesAction}>WorkerPreferences</a></li>
                <li class="breadcrumb-item active">New WorkerPreference</li>
            </ol>
        </nav>
        <h1>New WorkerPreference</h1>
        {renderForm workerPreference}
    |]

renderForm :: WorkerPreference -> Html
renderForm workerPreference =
    formFor
        workerPreference
        [hsx|
    {(textField #personId)}
    {(textField #sendDailyReminderAt)}
    {submitButton}
|]
