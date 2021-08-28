module Tests.Web.Job.FetchEntityPredictionSpec where

import qualified Application.VertexAi.Client as Client
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Web.Job.FetchEntityPrediction

spec :: Spec
spec = do
    describe "buildPredictions" do
        it "translates a prediction response to a list of predictions" do
            let response =
                    Client.Response
                        { predictions =
                            [ Client.Prediction
                                { ids = ["1", "2"]
                                , displayNames = ["one", "two"]
                                , textSegmentStartOffsets = [1, 10]
                                , textSegmentEndOffsets = [3, 13]
                                , confidences = [0.95, 0.96]
                                }
                            ]
                        , deployedModelId = "123"
                        }

            let predictions = buildPredictions response
            let [prediction1, prediction2] = case predictions of
                    p1 : p2 : ps -> [p1, p2]
                    _ -> error "Expected two predictions"

            get #vertexAiId prediction1 `shouldBe` "1"
            get #displayName prediction1 `shouldBe` "one"
            get #segmentStartOffset prediction1 `shouldBe` 1
            get #segmentEndOffset prediction1 `shouldBe` 3
            get #confidence prediction1 `shouldBe` 0.95

            get #vertexAiId prediction2 `shouldBe` "2"
            get #displayName prediction2 `shouldBe` "two"
            get #segmentStartOffset prediction2 `shouldBe` 10
            get #segmentEndOffset prediction2 `shouldBe` 13
            get #confidence prediction2 `shouldBe` 0.96

    describe "buildAssociations" do
        it "builds associations between the Twilio message and the predictions" do
            let twilioMessageId = "10000000-0000-0000-0000-000000000000"
            let prediction1 =
                    newRecord @VertexAiEntityPrediction
                        |> set #id "20000000-0000-0000-0000-000000000000"
            let prediction2 =
                    newRecord @VertexAiEntityPrediction
                        |> set #id "30000000-0000-0000-0000-000000000000"

            let associations = buildAssociations twilioMessageId [prediction1, prediction2]
            let [association1, association2] = case associations of
                    a1 : a2 : ps -> [a1, a2]
                    _ -> error "Expected two associations"

            get #twilioMessageId association1 `shouldBe` "10000000-0000-0000-0000-000000000000"
            get #vertexAiEntityPredictionId association1 `shouldBe` "20000000-0000-0000-0000-000000000000"

            get #twilioMessageId association2 `shouldBe` "10000000-0000-0000-0000-000000000000"
            get #vertexAiEntityPredictionId association2 `shouldBe` "30000000-0000-0000-0000-000000000000"
