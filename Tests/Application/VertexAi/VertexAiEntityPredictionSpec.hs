module Tests.Application.VertexAi.VertexAiEntityPredictionSpec where

import Application.VertexAi.VertexAiEntityPrediction
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "validate" do
        it "validates that vertexAiId is not blank" do
            newRecord @VertexAiEntityPrediction
                |> set #vertexAiId "  "
                |> set #displayName "hours_worked"
                |> validate
                |> get #meta
                |> get #annotations `shouldBe` [("vertexAiId", "This field cannot be blank")]

        it "validates that displayName is not blank" do
            newRecord @VertexAiEntityPrediction
                |> set #vertexAiId "123"
                |> set #displayName "  "
                |> validate
                |> get #meta
                |> get #annotations `shouldBe` [("displayName", "This field cannot be blank")]

        it "validates that segmentStartOffset is non-negative" do
            newRecord @VertexAiEntityPrediction
                |> set #vertexAiId "123"
                |> set #displayName "hours_worked"
                |> set #segmentStartOffset (-1)
                |> validate
                |> get #meta
                |> get #annotations `shouldBe` [("segmentStartOffset", "This field must be greater than or equal to 0")]

        it "validates that segmentEndOffset is greater than or equal to segmentStartOffset" do
            newRecord @VertexAiEntityPrediction
                |> set #vertexAiId "123"
                |> set #displayName "hours_worked"
                |> set #segmentStartOffset 1
                |> set #segmentEndOffset 0
                |> validate
                |> get #meta
                |> get #annotations `shouldBe` [("segmentEndOffset", "This field must be greater than or equal to segmentStartOffset")]

        it "validates that confidence is greater than or equal to 0.0" do
            newRecord @VertexAiEntityPrediction
                |> set #vertexAiId "123"
                |> set #displayName "hours_worked"
                |> set #confidence (-0.1)
                |> validate
                |> get #meta
                |> get #annotations `shouldBe` [("confidence", "This field must be greater than or equal to 0.0")]

        it "validates that confidence is less than than or equal to 1.0" do
            newRecord @VertexAiEntityPrediction
                |> set #vertexAiId "123"
                |> set #displayName "hours_worked"
                |> set #confidence 1.1
                |> validate
                |> get #meta
                |> get #annotations `shouldBe` [("confidence", "This field must be less than or equal to 1.0")]

        it "accepts records that pass all validations" do
            newRecord @VertexAiEntityPrediction
                |> set #vertexAiId "123"
                |> set #displayName "hours_worked"
                |> set #segmentStartOffset 1
                |> set #segmentEndOffset 2
                |> set #confidence 0.99
                |> validate
                |> get #meta
                |> get #annotations `shouldBe` []
