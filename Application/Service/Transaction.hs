module Application.Service.Transaction (
    withTransactionOrSavepoint,
) where

import qualified Database.PostgreSQL.Simple as PG
import IHP.Environment
import IHP.ModelSupport
import IHP.Prelude

withTransactionOrSavepoint ::
    (?modelContext :: ModelContext) =>
    ((?modelContext :: ModelContext) => IO a) ->
    IO a
withTransactionOrSavepoint block = withTransactionConnection do
    let connection =
            ?modelContext
                |> get #transactionConnection
                |> \case
                    Just connection -> connection
                    Nothing -> error "withTransactionOrSavepoint: transactionConnection not set as expected"

    inProgress <- isTransactionInProgress
    if inProgress
        then PG.withSavepoint connection block
        else PG.withTransaction connection block

isTransactionInProgress :: (?modelContext :: ModelContext) => IO Bool
isTransactionInProgress =
    sqlQueryScalar "select now() <> statement_timestamp();" ()