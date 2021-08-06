module Application.People.QuerySpec where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.People.Query as People.Query
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "fetchExcludingBot" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "excludes Matt Killam (TODO: this behavior temporary until we have a way to deactivate people)" do
                bot <-
                    newRecord @Person
                        |> set #firstName "Tim"
                        |> set #lastName "Eckard"
                        |> set #goesBy "Tim the Bot"
                        |> createRecord

                botPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id bot)
                    |> set #phoneNumberId (get #id botPhoneNumber)
                    |> createRecord

                matt <-
                    newRecord @Person
                        |> set #firstName "Matt"
                        |> set #lastName "Killam"
                        |> set #goesBy "Matt"
                        |> createRecord

                mattPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id matt)
                    |> set #phoneNumberId (get #id mattPhoneNumber)
                    |> createRecord

                people <- People.Query.fetchExcludingBot
                people `shouldBe` []

            itIO "selects everyone but the bot" do
                bot <-
                    newRecord @Person
                        |> set #firstName "Tim"
                        |> set #lastName "Eckard"
                        |> set #goesBy "Tim the Bot"
                        |> createRecord

                botPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id bot)
                    |> set #phoneNumberId (get #id botPhoneNumber)
                    |> createRecord

                donald <-
                    newRecord @Person
                        |> set #firstName "Donald"
                        |> set #lastName "Duck"
                        |> set #goesBy "Don"
                        |> createRecord

                donaldPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id donald)
                    |> set #phoneNumberId (get #id donaldPhoneNumber)
                    |> createRecord

                people <- People.Query.fetchExcludingBot

                people
                    `shouldBe` [ People.Query.Row
                                    { id = get #id donald
                                    , firstName = "Donald"
                                    , lastName = "Duck"
                                    , goesBy = "Don"
                                    , sendMessageActionState = Nothing
                                    }
                               ]

            itIO "orders people by last name alphabetically" do
                meyer <-
                    newRecord @Person
                        |> set #firstName "Oscar"
                        |> set #lastName "Meyer"
                        |> set #goesBy "Weiner"
                        |> createRecord

                meyerPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+17777777777"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id meyer)
                    |> set #phoneNumberId (get #id meyerPhoneNumber)
                    |> createRecord

                duck <-
                    newRecord @Person
                        |> set #firstName "Donald"
                        |> set #lastName "Duck"
                        |> set #goesBy "Don"
                        |> createRecord

                duckPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id duck)
                    |> set #phoneNumberId (get #id duckPhoneNumber)
                    |> createRecord

                appleton <-
                    newRecord @Person
                        |> set #firstName "Zoro"
                        |> set #lastName "Appleton"
                        |> set #goesBy "Z-boy"
                        |> createRecord

                appletonPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+16666666666"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id appleton)
                    |> set #phoneNumberId (get #id appletonPhoneNumber)
                    |> createRecord

                people <- People.Query.fetchExcludingBot

                get #lastName <$> people
                    `shouldBe` ["Appleton", "Duck", "Meyer"]

            itIO "includes the state of any send message action that has not been started" do
                donald <-
                    newRecord @Person
                        |> set #firstName "Donald"
                        |> set #lastName "Duck"
                        |> set #goesBy "Don"
                        |> createRecord

                donaldPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id donald)
                    |> set #phoneNumberId (get #id donaldPhoneNumber)
                    |> createRecord

                botPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                notStartedActionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> createRecord

                newRecord @SendMessageAction
                    |> set #actionRunStateId (get #id notStartedActionRunState)
                    |> set #fromId (get #id botPhoneNumber)
                    |> set #toId (get #id donaldPhoneNumber)
                    |> set #body "Hi!"
                    |> createRecord

                people <- People.Query.fetchExcludingBot

                people
                    `shouldBe` [ People.Query.Row
                                    { id = get #id donald
                                    , firstName = "Donald"
                                    , lastName = "Duck"
                                    , goesBy = "Don"
                                    , sendMessageActionState = Just ActionRunState.notStarted
                                    }
                               ]

            itIO "includes the state of any send message action that has been suspended" do
                donald <-
                    newRecord @Person
                        |> set #firstName "Donald"
                        |> set #lastName "Duck"
                        |> set #goesBy "Don"
                        |> createRecord

                donaldPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id donald)
                    |> set #phoneNumberId (get #id donaldPhoneNumber)
                    |> createRecord

                botPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                suspendedActionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.suspended
                        |> createRecord

                newRecord @SendMessageAction
                    |> set #actionRunStateId (get #id suspendedActionRunState)
                    |> set #fromId (get #id botPhoneNumber)
                    |> set #toId (get #id donaldPhoneNumber)
                    |> set #body "Hi!"
                    |> createRecord

                people <- People.Query.fetchExcludingBot

                people
                    `shouldBe` [ People.Query.Row
                                    { id = get #id donald
                                    , firstName = "Donald"
                                    , lastName = "Duck"
                                    , goesBy = "Don"
                                    , sendMessageActionState = Just ActionRunState.suspended
                                    }
                               ]

            itIO "does not include the state of send message actions that are in any other state" do
                donald <-
                    newRecord @Person
                        |> set #firstName "Donald"
                        |> set #lastName "Duck"
                        |> set #goesBy "Don"
                        |> createRecord

                donaldPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id donald)
                    |> set #phoneNumberId (get #id donaldPhoneNumber)
                    |> createRecord

                botPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                canceledActionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.canceled
                        |> createRecord

                newRecord @SendMessageAction
                    |> set #actionRunStateId (get #id canceledActionRunState)
                    |> set #fromId (get #id botPhoneNumber)
                    |> set #toId (get #id donaldPhoneNumber)
                    |> set #body "Hi!"
                    |> createRecord

                finishedActionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.finished
                        |> createRecord

                newRecord @SendMessageAction
                    |> set #actionRunStateId (get #id finishedActionRunState)
                    |> set #fromId (get #id botPhoneNumber)
                    |> set #toId (get #id donaldPhoneNumber)
                    |> set #body "Hi!"
                    |> createRecord

                failedActionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.failed
                        |> createRecord

                newRecord @SendMessageAction
                    |> set #actionRunStateId (get #id failedActionRunState)
                    |> set #fromId (get #id botPhoneNumber)
                    |> set #toId (get #id donaldPhoneNumber)
                    |> set #body "Hi!"
                    |> createRecord

                people <- People.Query.fetchExcludingBot

                people
                    `shouldBe` [ People.Query.Row
                                    { id = get #id donald
                                    , firstName = "Donald"
                                    , lastName = "Duck"
                                    , goesBy = "Don"
                                    , sendMessageActionState = Nothing
                                    }
                               ]

            itIO "the suspended state takes precedence over the not started state" do
                donald <-
                    newRecord @Person
                        |> set #firstName "Donald"
                        |> set #lastName "Duck"
                        |> set #goesBy "Don"
                        |> createRecord

                donaldPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id donald)
                    |> set #phoneNumberId (get #id donaldPhoneNumber)
                    |> createRecord

                botPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                suspendedActionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.suspended
                        |> createRecord

                newRecord @SendMessageAction
                    |> set #actionRunStateId (get #id suspendedActionRunState)
                    |> set #fromId (get #id botPhoneNumber)
                    |> set #toId (get #id donaldPhoneNumber)
                    |> set #body "Hi!"
                    |> createRecord

                notStartedActionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> createRecord

                newRecord @SendMessageAction
                    |> set #actionRunStateId (get #id notStartedActionRunState)
                    |> set #fromId (get #id botPhoneNumber)
                    |> set #toId (get #id donaldPhoneNumber)
                    |> set #body "Hi!"
                    |> createRecord

                people <- People.Query.fetchExcludingBot

                people
                    `shouldBe` [ People.Query.Row
                                    { id = get #id donald
                                    , firstName = "Donald"
                                    , lastName = "Duck"
                                    , goesBy = "Don"
                                    , sendMessageActionState = Just ActionRunState.suspended
                                    }
                               ]
