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
    aroundAll (withApp RootApplication testConfig) do
        describe "processIncomingMessage" do
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

                Brain.Process.processIncomingMessage
                    "https://timecard.company.com"
                    twilioMessage

                actionRunState <- fetch $ get #actionRunStateId sendMessageAction
                get #state actionRunState `shouldBe` ActionRunState.suspended
