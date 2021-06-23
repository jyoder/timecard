module Web.Controller.TimecardReviews where

import qualified Application.Timecard.TimecardQueries as TimecardQueries
import Network.Wai (remoteHost)
import Web.Controller.Prelude
import Web.View.TimecardReviews.Show

instance Controller TimecardReviewsController where
    action ShowTimecardReviewAction {..} = do
        now <- getCurrentTime

        maybeAccessToken <-
            query @AccessToken
                |> filterWhere (#value, accessToken)
                |> fetchOneOrNothing

        reviewStatus <- case maybeAccessToken of
            Just accessToken ->
                if now < get #expiresAt accessToken && not (get #isRevoked accessToken)
                    then do
                        timecardAccessToken <-
                            query @TimecardAccessToken
                                |> filterWhere (#accessTokenId, get #id accessToken)
                                |> fetchOne

                        timecard <-
                            TimecardQueries.fetchById
                                TimecardQueries.EntriesDateAscending
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
        -- TODO: ensure there are no existing signings

        let name = param @Text "name"
        let accessTokenValue = param @Text "accessTokenValue"
        let ipAddress = show $ remoteHost request
        now <- getCurrentTime

        accessToken <-
            query @AccessToken
                |> filterWhere (#value, accessTokenValue)
                |> fetchOne

        timecardAccessToken <-
            query @TimecardAccessToken
                |> filterWhere (#accessTokenId, get #id accessToken)
                |> fetchOne

        timecard <-
            TimecardQueries.fetchById
                TimecardQueries.EntriesDateAscending
                (get #timecardId timecardAccessToken)

        person <- fetch (get #personId timecard)

        withTransaction do
            newRecord @Signing
                |> set #name name
                |> set #signedAt now
                |> set #ipAddress ipAddress
                |> validateSigning
                |> ifValid \case
                    Left signing ->
                        let reviewStatus = ReviewFound {..}
                         in render ShowView {..}
                    Right signing -> do
                        signing <- createRecord signing
                        newRecord @TimecardSigning
                            |> set #timecardId (get #id timecard)
                            |> set #signingId (get #id signing)
                            |> createRecord
                            >> pure ()

        redirectTo $ ShowTimecardReviewAction accessTokenValue

validateSigning :: Signing -> Signing
validateSigning signing =
    signing
        |> validateField #name nonEmpty