module Web.Job.FetchEntityPrediction (
    buildPredictions,
    buildAssociations,
) where

import qualified Application.Brain.Process as Brain.Process
import Application.Service.Transaction (withTransactionOrSavepoint)
import Application.Service.Validation (validateAndCreate)
import qualified Application.Twilio.TwilioMessageEntity as TwilioMessageEntity
import qualified Application.VertexAi.Client as Client
import qualified Application.VertexAi.VertexAiEntityPrediction as VertexAiEntityPrediction
import qualified IHP.Log as Log
import Web.Controller.Prelude

instance Job FetchEntityPredictionJob where
    maxAttempts = 1

    perform FetchEntityPredictionJob {..} = do
        now <- getCurrentTime
        twilioMessage <- fetch twilioMessageId

        Log.info ("Fetching entity predictions for TwilioMessage: " <> show twilioMessageId)
        fetchEntityPredictions now Client.config twilioMessage >> pure ()

        Brain.Process.processState $ get #fromId twilioMessage

fetchEntityPredictions ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    Client.Config ->
    TwilioMessage ->
    IO [TwilioMessageEntity]
fetchEntityPredictions now config twilioMessage = do
    response <- Client.predict config now $ get #body twilioMessage
    let predictions = buildPredictions response
    withTransactionOrSavepoint do
        predictions <- createPredictions predictions
        buildAssociations (get #id twilioMessage) predictions
            |> createAssociations

buildPredictions :: Client.Response -> [VertexAiEntityPrediction]
buildPredictions Client.Response {..} =
    case predictions of
        Client.Prediction {..} : _ ->
            buildPrediction
                <$> zip5
                    ids
                    displayNames
                    textSegmentStartOffsets
                    textSegmentEndOffsets
                    confidences
        [] -> []
  where
    buildPrediction (vertexAiId, displayName, segmentStartOffset, segmentEndOffset, confidence) =
        newRecord @VertexAiEntityPrediction
            |> set #vertexAiId vertexAiId
            |> set #displayName displayName
            |> set #segmentStartOffset segmentStartOffset
            |> set #segmentEndOffset segmentEndOffset
            |> set #confidence confidence
            |> set #deployedModelId deployedModelId

createPredictions ::
    (?modelContext :: ModelContext) =>
    [VertexAiEntityPrediction] ->
    IO [VertexAiEntityPrediction]
createPredictions = mapM (validateAndCreate VertexAiEntityPrediction.validate)

buildAssociations :: Id TwilioMessage -> [VertexAiEntityPrediction] -> [TwilioMessageEntity]
buildAssociations twilioMessageId predictions = buildAssociation <$> predictions
  where
    buildAssociation prediction =
        newRecord @TwilioMessageEntity
            |> set #twilioMessageId twilioMessageId
            |> set #vertexAiEntityPredictionId (get #id prediction)

createAssociations ::
    (?modelContext :: ModelContext) =>
    [TwilioMessageEntity] ->
    IO [TwilioMessageEntity]
createAssociations = mapM (validateAndCreate TwilioMessageEntity.validate)

-- Update production environment variables
-- Update production Config.hs files
-- Ensure production is migrated (we changed some of the check constraints)