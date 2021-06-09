module Application.Service.People (
    validate,
    fetchExcluding,
    fetchBotId,
    fetchBot,
) where

import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ValidationSupport.ValidateField

validate :: Person -> Person
validate person =
    person
        |> validateField #firstName nonEmpty
        |> validateField #lastName nonEmpty
        |> validateField #goesBy nonEmpty

fetchExcluding ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO [Person]
fetchExcluding idToExclude = do
    people <- query @Person |> orderByAsc #lastName |> fetch
    filter (\person -> get #id person /= idToExclude) people |> pure

fetchBotId ::
    (?modelContext :: ModelContext) =>
    IO (Id Person)
fetchBotId = get #id <$> fetchBot

fetchBot ::
    (?modelContext :: ModelContext) =>
    IO Person
fetchBot = query @Person |> filterWhere (#goesBy, botName) |> fetchOne

botName :: Text
botName = "Tim the Bot"