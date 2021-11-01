module Web.View.AuditEntries.Index where

import Web.View.Prelude

data IndexView = IndexView {auditEntries :: [AuditEntry]}

instance View IndexView where
    html IndexView {..} =
        [hsx|
        <h1>Index</h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>AuditEntry</th>
                    </tr>
                </thead>
                <tbody>{forEach auditEntries renderAuditEntry}</tbody>
            </table>
        </div>
    |]

renderAuditEntry :: AuditEntry -> Html
renderAuditEntry auditEntry =
    [hsx|
    <tr>
        <td>{auditEntry}</td>
    </tr>
|]
