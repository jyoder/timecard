module Application.Timecard.TimecardAccessToken (
    validate,
    create,
    expirationFrom,
) where

import qualified Application.Base.AccessToken as AccessToken
import Application.Service.Validation
import Generated.Types hiding (expiresAt)
import IHP.ControllerPrelude hiding (create)

validate :: TimecardAccessToken -> TimecardAccessToken
validate timecardAccessToken = timecardAccessToken

create ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    Id Timecard ->
    IO TimecardAccessToken
create expiresAt timecardId =
    withTransaction do
        accessToken <-
            newRecord @AccessToken
                |> AccessToken.create expiresAt
        newRecord @TimecardAccessToken
            |> set #timecardId timecardId
            |> set #accessTokenId (get #id accessToken)
            |> validateAndCreate validate

expirationFrom :: UTCTime -> UTCTime
expirationFrom = threeWeeksFrom

threeWeeksFrom :: UTCTime -> UTCTime
threeWeeksFrom = addUTCTime threeWeeks

threeWeeks :: NominalDiffTime
threeWeeks = 7 * 3
