module Web.View.WorkerSettings.Show where

import Web.View.Prelude

data ShowView = ShowView {workerSetting :: WorkerSetting}

instance View ShowView where
    html ShowView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkerSettingsAction}>WorkerSettings</a></li>
                <li class="breadcrumb-item active">Show WorkerSetting</li>
            </ol>
        </nav>
        <h1>Show WorkerSetting</h1>
        <p>{workerSetting}</p>
    |]
