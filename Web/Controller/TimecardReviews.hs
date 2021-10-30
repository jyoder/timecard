module Web.Controller.TimecardReviews where

import qualified Application.Base.AccessToken as AccessToken
import qualified Application.Base.AuditEntry as AuditEntry
import qualified Application.Base.PhoneNumber as PhoneNumber
import qualified Application.Base.Signing as Signing
import qualified Application.People.Person as Person
import Application.Service.Transaction (withTransactionOrSavepoint)
import qualified Application.Timecard.AccessToken as Timecard.AccessToken
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Timecard.Signing as Timecard.Signing
import qualified Application.Timecard.View as Timecard.View
import Data.Maybe (fromJust)
import Network.Wai (remoteHost)
import Web.Controller.Prelude
import Web.View.TimecardReviews.Show

instance Controller TimecardReviewsController where
    action ShowTimecardReviewAction {..} = do
        maybeAccessToken <-
            query @AccessToken
                |> filterWhere (#value, accessToken)
                |> fetchOneOrNothing
        review <- case maybeAccessToken of
            Just accessToken -> do
                now <- getCurrentTime
                if AccessToken.isValidAsOf now accessToken
                    then do
                        timecardAccessToken <-
                            query @TimecardAccessToken
                                |> filterWhere (#accessTokenId, get #id accessToken)
                                |> fetchOne

                        timecard <-
                            fromJust . Timecard.View.buildTimecard
                                <$> Timecard.Query.fetchById
                                    Timecard.Query.EntriesDateAscending
                                    (get #timecardId timecardAccessToken)

                        person <- fetch $ get #personId timecard

                        maybeTimecardSigning <-
                            query @TimecardSigning
                                |> filterWhere (#timecardId, get #id timecard)
                                |> fetchOneOrNothing

                        signing <- case maybeTimecardSigning of
                            Just timecardSigning ->
                                query @Signing
                                    |> filterWhere (#id, get #signingId timecardSigning)
                                    |> fetchOne
                            Nothing -> pure $ newRecord @Signing

                        pure $ ReviewFound {..}
                    else pure ReviewExpired
            Nothing -> pure ReviewNotFound

        render ShowView {..}
    --
    action CreateSigningAction = do
        let name = param @Text "name"
        let accessTokenValue = param @Text "accessTokenValue"
        let ipAddress = show $ remoteHost request

        accessToken <-
            query @AccessToken
                |> filterWhere (#value, accessTokenValue)
                |> fetchOne

        timecardAccessToken <-
            query @TimecardAccessToken
                |> filterWhere (#accessTokenId, get #id accessToken)
                |> fetchOne

        timecard <-
            fromJust . Timecard.View.buildTimecard
                <$> Timecard.Query.fetchById
                    Timecard.Query.EntriesDateAscending
                    (get #timecardId timecardAccessToken)

        person <- fetch (get #personId timecard)
        phoneNumber <- PhoneNumber.fetchByPerson $ get #id person

        withTransactionOrSavepoint do
            now <- getCurrentTime
            newRecord @Signing
                |> set #name name
                |> set #signedAt now
                |> set #ipAddress ipAddress
                |> Signing.validate
                |> ifValid \case
                    Left signing ->
                        let review = ReviewFound {..}
                         in render ShowView {..}
                    Right signing -> do
                        signing <- createRecord signing
                        let timecardId = get #id timecard
                        let signingId = get #id signing
                        Timecard.Signing.create timecardId signingId
                        AuditEntry.createReviewSignedEntry (get #id phoneNumber) name
                        pure ()

        redirectTo $ ShowTimecardReviewAction accessTokenValue
