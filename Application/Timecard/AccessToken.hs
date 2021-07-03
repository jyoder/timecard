module Application.Timecard.AccessToken (
    create,
    validate,
    expirationFrom,
) where

import qualified Application.Base.AccessToken as AccessToken
import Application.Service.Transaction (withTransactionOrSavepoint)
import Application.Service.Validation
import Generated.Types hiding (expiresAt)
import IHP.ControllerPrelude hiding (create)

create ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    Id Timecard ->
    IO TimecardAccessToken
create expiresAt timecardId =
    withTransactionOrSavepoint do
        accessToken <-
            newRecord @AccessToken
                |> AccessToken.create expiresAt
        newRecord @TimecardAccessToken
            |> set #timecardId timecardId
            |> set #accessTokenId (get #id accessToken)
            |> validateAndCreate validate

validate :: TimecardAccessToken -> TimecardAccessToken
validate timecardAccessToken = timecardAccessToken

expirationFrom :: UTCTime -> UTCTime
expirationFrom = threeWeeksFrom

threeWeeksFrom :: UTCTime -> UTCTime
threeWeeksFrom = addUTCTime threeWeeks

threeWeeks :: NominalDiffTime
threeWeeks = 60 * 60 * 24 * 7 * 3
