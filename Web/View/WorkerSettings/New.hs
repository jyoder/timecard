module Web.View.WorkerSettings.New where

import Web.View.Prelude

data NewView = NewView {workerSetting :: WorkerSetting}

instance View NewView where
    html NewView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkerSettingsAction}>WorkerSettings</a></li>
                <li class="breadcrumb-item active">New WorkerSetting</li>
            </ol>
        </nav>
        <h1>New WorkerSetting</h1>
        {renderForm workerSetting}
    |]

renderForm :: WorkerSetting -> Html
renderForm workerSetting =
    formFor
        workerSetting
        [hsx|
    {(textField #personId)}
    {(textField #sendDailyReminderAt)}
    {submitButton}
|]
