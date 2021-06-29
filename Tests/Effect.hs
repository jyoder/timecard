module Tests.Effect (
    withTransactionRollback,
) where

import Control.Exception (finally)
import IHP.ControllerPrelude

withTransactionRollback ::
    forall a.
    (?modelContext :: ModelContext) =>
    ((?modelContext :: ModelContext) => IO a) ->
    IO a
withTransactionRollback f =
    withTransaction $
        finally f rollbackTransaction