module Web.Controller.TimecardReviews where

import qualified Application.Base.AccessToken as AccessToken
import qualified Application.Base.People as People
import qualified Application.Base.Signing as Signing
import qualified Application.Timecard.AccessToken as Timecard.AccessToken
import qualified Application.Timecard.Queries as Timecard.Queries
import qualified Application.Timecard.TimecardSigning as TimecardSigning
import Network.Wai (remoteHost)
import Web.Controller.Prelude
import Web.View.TimecardReviews.Show

instance Controller TimecardReviewsController where
    action ShowTimecardReviewAction {..} = do
        maybeAccessToken <-
            query @AccessToken
                |> filterWhere (#value, accessToken)
                |> fetchOneOrNothing
        reviewStatus <- case maybeAccessToken of
            Just accessToken -> do
                now <- getCurrentTime
                if AccessToken.isValidAsOf now accessToken
                    then do
                        timecardAccessToken <-
                            query @TimecardAccessToken
                                |> filterWhere (#accessTokenId, get #id accessToken)
                                |> fetchOne

                        timecard <-
                            Timecard.Queries.fetchById
                                Timecard.Queries.EntriesDateAscending
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
            Timecard.Queries.fetchById
                Timecard.Queries.EntriesDateAscending
                (get #timecardId timecardAccessToken)

        person <- fetch (get #personId timecard)

        withTransaction do
            now <- getCurrentTime
            newRecord @Signing
                |> set #name name
                |> set #signedAt now
                |> set #ipAddress ipAddress
                |> Signing.validate
                |> ifValid \case
                    Left signing ->
                        let reviewStatus = ReviewFound {..}
                         in render ShowView {..}
                    Right signing -> do
                        signing <- createRecord signing
                        let timecardId = get #id timecard
                        let signingId = get #id signing
                        TimecardSigning.create timecardId signingId >> pure ()

        redirectTo $ ShowTimecardReviewAction accessTokenValue
