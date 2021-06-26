module Application.Base.People (
    validate,
    fetchExcludingId,
    fetchBotId,
    fetchBot,
    fetchByPhoneNumber,
) where

import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ValidationSupport.ValidateField

fetchExcludingId :: (?modelContext :: ModelContext) => Id Person -> IO [Person]
fetchExcludingId idToExclude = do
    people <- query @Person |> orderByAsc #lastName |> fetch
    filter (\person -> get #id person /= idToExclude) people |> pure

fetchBotId :: (?modelContext :: ModelContext) => IO (Id Person)
fetchBotId = get #id <$> fetchBot

fetchBot :: (?modelContext :: ModelContext) => IO Person
fetchBot = query @Person |> filterWhere (#goesBy, botName) |> fetchOne

fetchByPhoneNumber ::
    (?modelContext :: ModelContext) =>
    Id PhoneNumber ->
    IO Person
fetchByPhoneNumber phoneNumberId = do
    phoneContact <-
        query @PhoneContact
            |> filterWhere (#phoneNumberId, phoneNumberId)
            |> fetchOne
    fetchOne (get #personId phoneContact)

botName :: Text
botName = "Tim the Bot"

validate :: Person -> Person
validate person =
    person
        |> validateField #firstName nonEmpty
        |> validateField #lastName nonEmpty
        |> validateField #goesBy nonEmpty
