module Web.View.WorkerPreferences.Show where
import Web.View.Prelude

data ShowView = ShowView { workerPreference :: WorkerPreference }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={WorkerPreferencesAction}>WorkerPreferences</a></li>
                <li class="breadcrumb-item active">Show WorkerPreference</li>
            </ol>
        </nav>
        <h1>Show WorkerPreference</h1>
        <p>{workerPreference}</p>
    |]
