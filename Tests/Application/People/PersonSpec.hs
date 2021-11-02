module Tests.Application.People.PersonSpec where

import qualified Application.People.Person as Person
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "fetchBotId" do
            itIO "selects the bot id" do
                bot <-
                    newRecord @Person
                        |> set #firstName "Tim"
                        |> set #lastName "Eckard"
                        |> set #goesBy "Tim the Bot"
                        |> createRecord

                newRecord @Person
                    |> set #firstName "Jim"
                    |> set #lastName "Neckard"
                    |> set #goesBy "I'm a real boy"
                    |> createRecord

                botId <- Person.fetchBotId
                botId `shouldBe` get #id bot

        describe "fetchByPhoneNumber" do
            itIO "selects a person that corresponds to a given phone number" do
                jim <-
                    newRecord @Person
                        |> set #firstName "Jim"
                        |> set #lastName "Neckard"
                        |> set #goesBy "Jim"
                        |> createRecord

                jimPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id jim)
                    |> set #phoneNumberId (get #id jimPhoneNumber)
                    |> createRecord

                sally <-
                    newRecord @Person
                        |> set #firstName "Sally"
                        |> set #lastName "Smith"
                        |> set #goesBy "Sally"
                        |> createRecord

                sallyPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id sally)
                    |> set #phoneNumberId (get #id sallyPhoneNumber)
                    |> createRecord

                person <- Person.fetchByPhoneNumber $ get #id jimPhoneNumber
                get #id person `shouldBe` get #id jim

    describe "botGoesBy" do
        it "returns Tim the Bot" do
            Person.botGoesBy `shouldBe` "Tim the Bot"

    describe "validate" do
        it "ensures that the first name is non-empty" do
            let person =
                    newRecord @Person
                        |> set #firstName ""
                        |> set #lastName "Rogers"
                        |> set #goesBy "Mister"
            Person.validate person |> get #meta |> get #annotations
                `shouldBe` [("firstName", TextViolation "This field cannot be empty")]

        it "ensures that the last name is non-empty" do
            let person =
                    newRecord @Person
                        |> set #firstName "Fred"
                        |> set #lastName ""
                        |> set #goesBy "Mister"
            Person.validate person |> get #meta |> get #annotations
                `shouldBe` [("lastName", TextViolation "This field cannot be empty")]

        it "ensures that the goes-by field is non-empty" do
            let person =
                    newRecord @Person
                        |> set #firstName "Fred"
                        |> set #lastName "Rogers"
                        |> set #goesBy ""
            Person.validate person |> get #meta |> get #annotations
                `shouldBe` [("goesBy", TextViolation "This field cannot be empty")]
