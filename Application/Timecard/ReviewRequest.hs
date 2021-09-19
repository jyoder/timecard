module Application.Timecard.ReviewRequest where

import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Timecard.AccessToken as Timecard.AccessToken
import Generated.Types
import IHP.ControllerPrelude

scheduleRequest ::
    (?modelContext :: ModelContext) =>
    Text ->
    UTCTime ->
    Id Timecard ->
    Text ->
    Id PhoneNumber ->
    Id PhoneNumber ->
    IO SendMessageAction
scheduleRequest baseUrl now timecardId workerName fromPhoneNumberId toPhoneNumberId = do
    timecardAccessToken <- Timecard.AccessToken.create expiresAt timecardId
    accessToken <- fetchOne $ get #accessTokenId timecardAccessToken
    let link = reviewLink baseUrl (get #value accessToken)
        sendAt = requestTime now
        body = requestBody workerName link
     in SendMessageAction.schedule fromPhoneNumberId toPhoneNumberId body sendAt
  where
    expiresAt = Timecard.AccessToken.expirationFrom now

reviewLink :: Text -> Text -> Text
reviewLink baseUrl accessToken = baseUrl <> "/ShowTimecardReview?accessToken=" <> accessToken

requestTime :: UTCTime -> UTCTime
requestTime = addUTCTime (60 * 4)

requestBody :: Text -> Text -> Text
requestBody name link =
    "Thanks "
        <> name
        <> ". Here's your timecard to review and sign:\n"
        <> link
        <> "\n\nLet me know if you need me to make any corrections on it. Have a good weekend!"
