module Application.Timecard.ReviewRequest where

import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Base.AuditEntry as AuditEntry
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
    IO SendMessageAction
scheduleRequest userId baseUrl now timecardId worker fromPhoneNumberId toPhoneNumberId = do
    workerSettings <-
        query @WorkerSetting
            |> filterWhere (#personId, get #id worker)
            |> fetchOne
    let language = WorkerSettings.toLanguage $ get #preferredLanguage workerSettings

    timecardAccessToken <- Timecard.AccessToken.create expiresAt timecardId
    accessToken <- fetchOne $ get #accessTokenId timecardAccessToken

    let link = reviewLink baseUrl (get #value accessToken)
        sendAt = requestTime now
        body = requestBody language (get #goesBy worker) link
    AuditEntry.createReviewLinkGeneratedEntry userId toPhoneNumberId link

    sendMessageAction <- SendMessageAction.schedule fromPhoneNumberId toPhoneNumberId body sendAt
    AuditEntry.createReviewRequestScheduledEntry sendMessageAction sendAt
    pure sendMessageAction
  where
    expiresAt = Timecard.AccessToken.expirationFrom now

reviewLink :: Text -> Text -> Text
reviewLink baseUrl accessToken = baseUrl <> "/ShowTimecardReview?accessToken=" <> accessToken

requestTime :: UTCTime -> UTCTime
requestTime = addUTCTime (60 * 4)

requestBody :: Language -> Text -> Text -> Text
requestBody English name link =
    "Thanks "
        <> name
        <> ". Here's your timecard to review and sign:\n"
        <> link
        <> "\n\nLet me know if you need me to make any corrections on it."
requestBody Spanish name link =
    "Gracias "
        <> name
        <> ". Aquí está su tarjeta de tiempo para revisar y firmar:\n"
        <> link
        <> "\n\nAvíseme si necesita que le haga alguna corrección."
