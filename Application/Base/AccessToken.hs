module Application.Base.AccessToken (
    validate,
    create,
) where

import Application.Service.Validation
import Generated.Types
import IHP.ControllerPrelude hiding (create)

validate :: AccessToken -> AccessToken
validate accessToken =
    accessToken
        |> validateField #value nonEmpty

create ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    AccessToken ->
    IO AccessToken
create expiresAt accessToken = do
    value <- generateAuthenticationToken
    accessToken
        |> set #expiresAt expiresAt
        |> set #value value
        |> validateAndCreate validate
