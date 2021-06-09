module Application.Service.People where

import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

fetchPeopleExcluding ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO [Person]
fetchPeopleExcluding idToExclude = do
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