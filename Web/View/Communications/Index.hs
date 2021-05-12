module Web.View.Communications.Index where
import Web.View.Prelude

data IndexView = IndexView

instance View IndexView where
    html IndexView = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={CommunicationsAction}>Communications</a></li>
            </ol>
        </nav>
        <h1>Index</h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Communication</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach [] renderCommunication}</tbody>
            </table>
        </div>
    |]


renderCommunication communication = [hsx|
    <tr>
        <td>communication</td>
        <td><a href="#">Show</a></td>
        <td><a href="#" class="text-muted">Edit</a></td>
        <td><a href="#" class="js-delete text-muted">Delete</a></td>
    </tr>
|]
