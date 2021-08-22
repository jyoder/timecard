module Tests.Application.People.ViewSpec where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.People.Query as People.Query
import qualified Application.People.View as People.View
import IHP.Prelude
import Test.Hspec

spec :: Spec
spec = do
    describe "buildPeople" do
        it "returns a list of people based on the given parameters" do
            let rows =
                    [ People.Query.Row
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Bob"
                        , lastName = "Bobbers"
                        , goesBy = "Gortock"
                        , sendMessageActionState = Just ActionRunState.notStarted
                        }
                    , People.Query.Row
                        { id = "20000000-0000-0000-0000-000000000000"
                        , firstName = "Rob"
                        , lastName = "Robbers"
                        , goesBy = "Robo"
                        , sendMessageActionState = Just ActionRunState.suspended
                        }
                    ]

            People.View.buildPeople rows
                `shouldBe` [ People.View.Person
                                { id = "10000000-0000-0000-0000-000000000000"
                                , firstName = "Bob"
                                , lastName = "Bobbers"
                                , goesBy = "Gortock"
                                , state = People.View.PersonAutoPilot
                                }
                           , People.View.Person
                                { id = "20000000-0000-0000-0000-000000000000"
                                , firstName = "Rob"
                                , lastName = "Robbers"
                                , goesBy = "Robo"
                                , state = People.View.PersonNeedsAttention
                                }
                           ]

    describe "buildPerson" do
        it "returns a person based on the given parameters" do
            let row =
                    People.Query.Row
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Bob"
                        , lastName = "Bobbers"
                        , goesBy = "Gortock"
                        , sendMessageActionState = Just ActionRunState.notStarted
                        }

            People.View.buildPerson row
                `shouldBe` People.View.Person
                    { id = "10000000-0000-0000-0000-000000000000"
                    , firstName = "Bob"
                    , lastName = "Bobbers"
                    , goesBy = "Gortock"
                    , state = People.View.PersonAutoPilot
                    }

    describe "personState" do
        it "returns an idle state when there was no send message action" do
            People.View.personState Nothing `shouldBe` People.View.PersonIdle

        it "returns an idle state when the send message action was finished" do
            People.View.personState (Just ActionRunState.finished) `shouldBe` People.View.PersonIdle

        it "returns an idle state when the send message action was canceled" do
            People.View.personState (Just ActionRunState.canceled) `shouldBe` People.View.PersonIdle

        it "returns an idle state when the send message action was failed" do
            People.View.personState (Just ActionRunState.failed) `shouldBe` People.View.PersonIdle

        it "returns an auto-pilot state when the send message action was not started" do
            People.View.personState (Just ActionRunState.notStarted) `shouldBe` People.View.PersonAutoPilot

        it "returns a needs-attention state when the send message action was suspended" do
            People.View.personState (Just ActionRunState.suspended) `shouldBe` People.View.PersonNeedsAttention