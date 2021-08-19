module Web.View.WorkerSettings.Index where

import Web.View.Prelude

data IndexView = IndexView {workerSettings :: [WorkerSetting]}

instance View IndexView where
    html IndexView {..} =
        [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={WorkerSettingsAction}>WorkerSettings</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewWorkerSettingAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>WorkerSetting</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach workerSettings renderWorkerSetting}</tbody>
            </table>
        </div>
    |]

renderWorkerSetting :: WorkerSetting -> Html
renderWorkerSetting workerSetting =
    [hsx|
    <tr>
        <td>{workerSetting}</td>
        <td><a href={ShowWorkerSettingAction (get #id workerSetting)}>Show</a></td>
        <td><a href={EditWorkerSettingAction (get #id workerSetting)} class="text-muted">Edit</a></td>
        <td><a href={DeleteWorkerSettingAction (get #id workerSetting)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
