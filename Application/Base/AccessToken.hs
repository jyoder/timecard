module Application.Base.AccessToken (
    isValidAsOf,
    create,
    validate,
) where

import Application.Service.Validation
import Generated.Types
import IHP.ControllerPrelude hiding (create)

fetchValidByValue ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    Text ->
    IO (Maybe AccessToken)
fetchValidByValue time value = do
    maybeAccessToken <-
        query @AccessToken
            |> filterWhere (#value, value)
            |> fetchOneOrNothing
    case maybeAccessToken of
        Just accessToken ->
            if isValidAsOf time accessToken
                then pure $ Just accessToken
                else pure Nothing
        Nothing -> pure Nothing

isValidAsOf :: UTCTime -> AccessToken -> Bool
isValidAsOf time accessToken =
    time < get #expiresAt accessToken
        && not (get #isRevoked accessToken)

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

validate :: AccessToken -> AccessToken
validate accessToken =
    accessToken
        |> validateField #value nonEmpty
