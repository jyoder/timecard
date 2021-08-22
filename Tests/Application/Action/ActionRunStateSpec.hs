module Tests.Application.Action.ActionRunStateSpec where

import qualified Application.Action.ActionRunState as ActionRunState
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "updateNotStarted" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "updates the state to not_started" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state "running"
                        |> createRecord

                actionRunState <- ActionRunState.updateNotStarted actionRunState
                get #state actionRunState `shouldBe` "not_started"

    describe "updateSuspended" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "updates the state to suspended" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state "suspended"
                        |> createRecord

                actionRunState <- ActionRunState.updateSuspended actionRunState
                get #state actionRunState `shouldBe` "suspended"

    describe "updateRunning" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "updates the state to running" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state "running"
                        |> createRecord

                actionRunState <- ActionRunState.updateRunning actionRunState
                get #state actionRunState `shouldBe` "running"

    describe "updateFinished" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "updates the state to finished" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state "finished"
                        |> createRecord

                actionRunState <- ActionRunState.updateFinished actionRunState
                get #state actionRunState `shouldBe` "finished"

    describe "updateCanceled" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "updates the state to canceled" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state "canceled"
                        |> createRecord

                actionRunState <- ActionRunState.updateCanceled actionRunState
                get #state actionRunState `shouldBe` "canceled"

    describe "updateFailed" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "updates the state to failed" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state "failed"
                        |> createRecord

                actionRunState <- ActionRunState.updateFailed actionRunState
                get #state actionRunState `shouldBe` "failed"

    describe "validate" do
        it "disallows an unknown state" do
            newRecord @ActionRunState
                |> set #state "unknown"
                |> ActionRunState.validate
                |> ifValid \case
                    Left _ -> pure ()
                    Right _ -> expectationFailure "should disallow unknown state"

        it "accepts not_started as a valid state" do
            newRecord @ActionRunState
                |> set #state "not_started"
                |> ActionRunState.validate
                |> ifValid \case
                    Left _ -> expectationFailure "should allow not_started"
                    Right _ -> pure ()

        it "accepts suspended as a valid state" do
            newRecord @ActionRunState
                |> set #state "suspended"
                |> ActionRunState.validate
                |> ifValid \case
                    Left _ -> expectationFailure "should allow suspended"
                    Right _ -> pure ()

        it "accepts running as a valid state" do
            newRecord @ActionRunState
                |> set #state "running"
                |> ActionRunState.validate
                |> ifValid \case
                    Left _ -> expectationFailure "should allow running"
                    Right _ -> pure ()

        it "accepts finished as a valid state" do
            newRecord @ActionRunState
                |> set #state "finished"
                |> ActionRunState.validate
                |> ifValid \case
                    Left _ -> expectationFailure "should allow finished"
                    Right _ -> pure ()

        it "accepts canceled as a valid state" do
            newRecord @ActionRunState
                |> set #state "canceled"
                |> ActionRunState.validate
                |> ifValid \case
                    Left _ -> expectationFailure "should allow candeled"
                    Right _ -> pure ()

        it "accepts failed as a valid state" do
            newRecord @ActionRunState
                |> set #state "failed"
                |> ActionRunState.validate
                |> ifValid \case
                    Left _ -> expectationFailure "should allow failed"
                    Right _ -> pure ()

    describe "notStarted" do
        it "returns not_started" do
            ActionRunState.notStarted `shouldBe` "not_started"

    describe "suspended" do
        it "returns suspended" do
            ActionRunState.suspended `shouldBe` "suspended"

    describe "running" do
        it "returns running" do
            ActionRunState.running `shouldBe` "running"

    describe "finished" do
        it "returns finished" do
            ActionRunState.finished `shouldBe` "finished"

    describe "failed" do
        it "returns failed" do
            ActionRunState.failed `shouldBe` "failed"