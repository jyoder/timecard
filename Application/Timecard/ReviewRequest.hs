module Application.Timecard.ReviewRequest where

import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Audit.Entry as Audit.Entry
import Application.Base.WorkerSettings (Language (..))
import qualified Application.Base.WorkerSettings as WorkerSettings
import qualified Application.Timecard.AccessToken as Timecard.AccessToken
import Generated.Types
import IHP.ControllerPrelude

scheduleRequest ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    Text ->
    UTCTime ->
    Id Timecard ->
    Person ->
    Id PhoneNumber ->
    Id PhoneNumber ->
    IO (SendMessageAction, SendMessageAction)
scheduleRequest userId baseUrl now timecardId worker fromPhoneNumberId toPhoneNumberId = do
    workerSettings <-
        query @WorkerSetting
            |> filterWhere (#personId, get #id worker)
            |> fetchOne
    let language = WorkerSettings.toLanguage $ get #preferredLanguage workerSettings

    timecardAccessToken <- Timecard.AccessToken.create expiresAt timecardId
    accessToken <- fetchOne $ get #accessTokenId timecardAccessToken

    let link = reviewLink baseUrl (get #value accessToken)
        sendRequestAt = requestTime now
        sendLinkAt = linkTime now
        body = requestBody language (get #goesBy worker)
    Audit.Entry.createReviewLinkGenerated userId toPhoneNumberId link

    requestMessage <- SendMessageAction.schedule fromPhoneNumberId toPhoneNumberId body sendRequestAt
    linkMessage <- SendMessageAction.schedule fromPhoneNumberId toPhoneNumberId link sendLinkAt
    Audit.Entry.createReviewRequestScheduled requestMessage sendRequestAt
    pure (requestMessage, linkMessage)
  where
    expiresAt = Timecard.AccessToken.expirationFrom now
    auditContext body link = body <> ": " <> link

requestTime :: UTCTime -> UTCTime
requestTime = addUTCTime (60 * 4)

requestBody :: Language -> Text -> Text
requestBody English name =
    "Thanks "
        <> name
        <> ". Here's your timecard to review and sign. Let me know if you need me to make any corrections on it:"
requestBody Spanish name =
    "Gracias "
        <> name
        <> ". Aquí está su tarjeta de tiempo para revisar y firmar. Avíseme si necesita que le haga alguna corrección:"

linkTime :: UTCTime -> UTCTime
linkTime now = addUTCTime 10 (requestTime now)

reviewLink :: Text -> Text -> Text
reviewLink baseUrl accessToken = baseUrl <> "/ShowTimecardReview?accessToken=" <> accessToken
