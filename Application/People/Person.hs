module Application.People.Person (
    fetchBotId,
    botGoesBy,
    fetchByPhoneNumber,
    validate,
) where

import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ValidationSupport.ValidateField

fetchBotId :: (?modelContext :: ModelContext) => IO (Id Person)
fetchBotId = get #id <$> fetchBot

fetchBot :: (?modelContext :: ModelContext) => IO Person
fetchBot = query @Person |> filterWhere (#goesBy, botGoesBy) |> fetchOne

botGoesBy :: Text
botGoesBy = "Tim the Bot"

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

validate :: Person -> Person
validate person =
    person
        |> validateField #firstName nonEmpty
        |> validateField #lastName nonEmpty
        |> validateField #goesBy nonEmpty
