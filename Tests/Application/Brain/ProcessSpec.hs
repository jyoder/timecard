module Tests.Application.Brain.ProcessSpec where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Brain.Process as Brain.Process
import Data.Set (elems)
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "processIncomingMessage" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "processes an incoming message, taking the appropriate action based on the brain's plan" do
                worker <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                bot <-
                    newRecord @Person
                        |> set #firstName "Tim"
                        |> set #lastName "Eckard"
                        |> set #goesBy "Tim the Bot"
                        |> createRecord

                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> createRecord

                actionRunTime <-
                    newRecord @ActionRunTime
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #runsAt (toUtc "2021-06-23 15:00:02 PDT")
                        |> createRecord

                workerPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                botPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #phoneNumberId (get #id botPhoneNumber)
                    |> set #personId (get #id bot)
                    |> createRecord

                newRecord @PhoneContact
                    |> set #phoneNumberId (get #id workerPhoneNumber)
                    |> set #personId (get #id worker)
                    |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #createdAt (toUtc "2021-06-23 15:00:01 PDT")
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id botPhoneNumber)
                        |> set #toId (get #id workerPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                twilioMessage <-
                    newRecord @TwilioMessage
                        |> set #createdAt (toUtc "2021-06-23 15:00:03 PDT")
                        |> set #fromId (get #id workerPhoneNumber)
                        |> set #toId (get #id botPhoneNumber)
                        |> set #apiVersion "1"
                        |> set #messageSid "2"
                        |> set #accountSid "3"
                        |> set #messagingServiceSid Nothing
                        |> set #status "received"
                        |> set #body "Hi!"
                        |> createRecord

                Brain.Process.processIncomingMessage twilioMessage

                actionRunState <- fetch $ get #actionRunStateId sendMessageAction
                get #state actionRunState `shouldBe` ActionRunState.suspended

            itIO "does not suspend send message actions for old received messages" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> createRecord

                actionRunTime <-
                    newRecord @ActionRunTime
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #runsAt (toUtc "2021-06-23 15:00:03 PDT")
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
                        |> set #createdAt (toUtc "2021-06-23 15:00:02 PDT")
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id toPhoneNumber)
                        |> set #toId (get #id fromPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                twilioMessage <-
                    newRecord @TwilioMessage
                        |> set #createdAt (toUtc "2021-06-23 15:00:01 PDT")
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #apiVersion "1"
                        |> set #messageSid "2"
                        |> set #accountSid "3"
                        |> set #messagingServiceSid Nothing
                        |> set #status "received"
                        |> set #body "Hi!"
                        |> createRecord

                Brain.Process.processState $ get #id fromPhoneNumber

                actionRunState <- fetch $ get #actionRunStateId sendMessageAction
                get #state actionRunState `shouldBe` ActionRunState.notStarted

            itIO "does not suspend send message actions if there are no received messages" do
                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> createRecord

                actionRunTime <-
                    newRecord @ActionRunTime
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #runsAt (toUtc "2021-06-23 15:00:03 PDT")
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
                        |> set #createdAt (toUtc "2021-06-23 15:00:02 PDT")
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id toPhoneNumber)
                        |> set #toId (get #id fromPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                Brain.Process.processState $ get #id fromPhoneNumber

                actionRunState <- fetch $ get #actionRunStateId sendMessageAction
                get #state actionRunState `shouldBe` ActionRunState.notStarted
