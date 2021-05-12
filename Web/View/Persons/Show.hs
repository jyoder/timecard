module Web.View.Persons.Show where
import Web.View.Prelude

data ShowView = ShowView { person :: Person }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PersonsAction}>Persons</a></li>
                <li class="breadcrumb-item active">Show Person</li>
            </ol>
        </nav>
        <h1>Show Person</h1>
        <p>{person}</p>
    |]
