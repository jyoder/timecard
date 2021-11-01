module Web.Controller.AuditEntries where

import Web.Controller.Prelude
import Web.View.AuditEntries.Edit
import Web.View.AuditEntries.Index
import Web.View.AuditEntries.New
import Web.View.AuditEntries.Show

instance Controller AuditEntriesController where
    action AuditEntriesAction = do
        auditEntries <- query @AuditEntry |> fetch
        render IndexView {..}
    --
    action AuditEntriesPersonSelectionAction {..} = do
        auditEntries <- query @AuditEntry |> fetch
        render IndexView {..}
