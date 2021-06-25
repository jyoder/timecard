module Web.View.WorkerPreferences.Index where
import Web.View.Prelude

data IndexView = IndexView { workerPreferences :: [WorkerPreference] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={WorkerPreferencesAction}>WorkerPreferences</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewWorkerPreferenceAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>WorkerPreference</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach workerPreferences renderWorkerPreference}</tbody>
            </table>
        </div>
    |]


renderWorkerPreference :: WorkerPreference -> Html
renderWorkerPreference workerPreference = [hsx|
    <tr>
        <td>{workerPreference}</td>
        <td><a href={ShowWorkerPreferenceAction (get #id workerPreference)}>Show</a></td>
        <td><a href={EditWorkerPreferenceAction (get #id workerPreference)} class="text-muted">Edit</a></td>
        <td><a href={DeleteWorkerPreferenceAction (get #id workerPreference)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
