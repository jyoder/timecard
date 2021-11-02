module Tests.Application.Action.SendMessageActionSpec where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import Application.Service.Validation (ValidationException (..))
import Data.Set (elems)
import Generated.Types
import IHP.Controller.RequestContext
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "validate" do
        it "allows a non-empty body" do
            newRecord @SendMessageAction
                |> set #body "non-empty"
                |> SendMessageAction.validate
                |> ifValid \case
                    Left _ -> expectationFailure "should allow running"
                    Right _ -> pure ()

        it "disallows an empty body" do
            newRecord @SendMessageAction
                |> set #body ""
                |> SendMessageAction.validate
                |> ifValid \case
                    Left _ -> pure ()
                    Right _ -> expectationFailure "should disallow empty body"

    aroundAll (withApp RootApplication testConfig) do
        describe "fetchReadyToRun" do
            itIO "returns a send message action if the run time is in the past and it has not been started" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> set #runsAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchReadyToRun $
                        toUtc "2021-06-23 15:00:00 PDT"

                get #id <$> sendMessageActions `shouldBe` [get #id sendMessageAction]

            itIO "does not return a send message action if the run time is in the future" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> set #runsAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchReadyToRun $
                        toUtc "2021-06-23 14:59:59 PDT"

                get #id <$> sendMessageActions `shouldBe` []

            itIO "does not return a send message action if it is running" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.running
                        |> set #runsAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchReadyToRun $
                        toUtc "2021-06-23 15:00:00 PDT"

                get #id <$> sendMessageActions `shouldBe` []

            itIO "does not return a send message action if it is canceled" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.canceled
                        |> set #runsAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchReadyToRun $
                        toUtc "2021-06-23 15:00:00 PDT"

                get #id <$> sendMessageActions `shouldBe` []

            itIO "does not return a send message action if it is suspended" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.suspended
                        |> set #runsAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchReadyToRun $
                        toUtc "2021-06-23 15:00:00 PDT"

                get #id <$> sendMessageActions `shouldBe` []

            itIO "tracks appropriate tables" do
                withTableReadTracker do
                    SendMessageAction.fetchReadyToRun $ toUtc "2021-06-23 15:00:00 PDT"
                    trackedTables <- readIORef ?touchedTables
                    elems trackedTables
                        `shouldBe` [ "action_run_states"
                                   , "send_message_actions"
                                   ]

        describe "fetchNotStartedOrSuspendedByPhoneNumber" do
            itIO "returns a send message action if it is not started" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> set #runsAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber $
                        get #id toPhoneNumber

                get #id <$> sendMessageActions `shouldBe` [get #id sendMessageAction]

            itIO "returns a send message action if it has been suspended" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.suspended
                        |> set #runsAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber $
                        get #id toPhoneNumber

                get #id <$> sendMessageActions `shouldBe` [get #id sendMessageAction]

            itIO "does not return a send message action if it is canceled" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.canceled
                        |> set #runsAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber $
                        get #id toPhoneNumber

                get #id <$> sendMessageActions `shouldBe` []

            itIO "tracks appropriate tables" do
                withTableReadTracker do
                    let toPhoneNumber = newRecord @PhoneNumber

                    SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber $
                        get #id toPhoneNumber

                    trackedTables <- readIORef ?touchedTables
                    elems trackedTables
                        `shouldBe` [ "action_run_states"
                                   , "send_message_actions"
                                   ]

        describe "fetchNotStartedCreatedBeforeByPhoneNumber" do
            itIO "returns a send message action if it is not started and created before the given time" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #createdAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchNotStartedCreatedBeforeByPhoneNumber
                        (toUtc "2021-06-23 15:00:01 PDT")
                        (get #id toPhoneNumber)

                get #id <$> sendMessageActions `shouldBe` [get #id sendMessageAction]

            itIO "does not return a send message action if it is not started and created after the given time" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #createdAt (toUtc "2021-06-23 15:00:01 PDT")
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchNotStartedCreatedBeforeByPhoneNumber
                        (toUtc "2021-06-23 15:00:00 PDT")
                        (get #id toPhoneNumber)

                get #id <$> sendMessageActions `shouldBe` []

            itIO "returns a send message action if it is running and created before the given time" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.running
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #createdAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                sendMessageActions <-
                    SendMessageAction.fetchNotStartedCreatedBeforeByPhoneNumber
                        (toUtc "2021-06-23 15:00:01 PDT")
                        (get #id toPhoneNumber)

                get #id <$> sendMessageActions `shouldBe` []

            itIO "tracks appropriate tables" do
                withTableReadTracker do
                    let toPhoneNumber = newRecord @PhoneNumber

                    SendMessageAction.fetchNotStartedCreatedBeforeByPhoneNumber
                        (toUtc "2021-06-23 15:00:01 PDT")
                        (get #id toPhoneNumber)

                    trackedTables <- readIORef ?touchedTables
                    elems trackedTables
                        `shouldBe` [ "action_run_states"
                                   , "send_message_actions"
                                   ]

        describe "schedule" do
            itIO "saves a send message action based on the given parameters" do
                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    SendMessageAction.schedule
                        (get #id fromPhoneNumber)
                        (get #id toPhoneNumber)
                        "Hello World!"
                        (toUtc "2021-06-23 15:00:00 PDT")

                actionRunState <-
                    fetch $ get #actionRunStateId sendMessageAction

                get #state actionRunState `shouldBe` ActionRunState.notStarted
                get #runsAt actionRunState `shouldBe` toUtc "2021-06-23 15:00:00 PDT"
                get #body sendMessageAction `shouldBe` "Hello World!"
                get #fromId sendMessageAction `shouldBe` get #id fromPhoneNumber
                get #toId sendMessageAction `shouldBe` get #id toPhoneNumber

            itIO "throws an exception if the send message action does not validate successfully" do
                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                SendMessageAction.schedule
                    (get #id fromPhoneNumber)
                    (get #id toPhoneNumber)
                    ""
                    (toUtc "2021-06-23 15:00:00 PDT")
                    `shouldThrow` (== ValidationException "body=TextViolation {message = \"This field cannot be empty\"}")

        describe "perform" do
            itIO "sends a message via twilio and saves the result in the database" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.running
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                let sendMessageAction' =
                        SendMessageAction.T
                            { id = get #id sendMessageAction
                            , actionRunStateId = get #id actionRunState
                            , state = get #state actionRunState
                            , runsAt = get #runsAt actionRunState
                            , body = get #body sendMessageAction
                            , fromId = get #id fromPhoneNumber
                            , fromNumber = get #number fromPhoneNumber
                            , toId = get #id toPhoneNumber
                            , toNumber = get #number toPhoneNumber
                            }

                let ?context = get #frameworkConfig ?context
                 in do
                        twilioMessage <- SendMessageAction.perform sendMessageAction'
                        get #apiVersion twilioMessage `shouldBe` "fakeApiVersion"
                        get #accountSid twilioMessage `shouldBe` "fakeAccountSid"
                        get #fromId twilioMessage `shouldBe` get #id fromPhoneNumber
                        get #toId twilioMessage `shouldBe` get #id toPhoneNumber
                        get #status twilioMessage `shouldBe` "delivered"
                        get #body twilioMessage `shouldBe` "Hello!"
                        get #numMedia twilioMessage `shouldBe` 0