module Application.Service.PhoneNumber (
    validate,
    fetchByPerson,
) where

import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ValidationSupport.ValidateField

validate :: PhoneNumber -> PhoneNumber
validate phoneNumber =
    phoneNumber
        |> validateField #number isPhoneNumber

fetchByPerson ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO PhoneNumber
fetchByPerson personId = do
    phoneContact <-
        query @PhoneContact
            |> filterWhere (#personId, personId)
            |> fetchOne
    fetchOne (get #phoneNumberId phoneContact)
