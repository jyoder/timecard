module Web.View.WorkerSettings.Edit where

import Web.View.Prelude

data EditView = EditView {workerSetting :: WorkerSetting}

instance View EditView where
    html EditView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkerSettingsAction}>WorkerSettings</a></li>
                <li class="breadcrumb-item active">Edit WorkerSetting</li>
            </ol>
        </nav>
        <h1>Edit WorkerSetting</h1>
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
