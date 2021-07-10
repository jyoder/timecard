module Web.View.Navigation.PeopleSpec where

import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Web.View.Navigation.People

newtype DummyController = DummyControllerAction
    { selectedPersonId :: Id Person
    }
    deriving (Eq, Show, Data)

instance Controller DummyController where
    action DummyControllerAction {..} = pure ()

instance AutoRoute DummyController

spec :: Spec
spec = do
    describe "buildPeopleNavigation" do
        it "returns a people navigation with the selected person shown as active" do
            let barbara =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "Barbara"
                        |> set #lastName "Bush"

            let jackie =
                    newRecord @Person
                        |> set #id "20000000-0000-0000-0000-000000000000"
                        |> set #firstName "Jackie"
                        |> set #lastName "Kennedy"

            let people = [barbara, jackie]

            buildPeopleNavigation
                DummyControllerAction
                (Just barbara)
                people
                `shouldBe` PeopleNavigation
                    { personItems =
                        [ PersonItem
                            { selectionAction =
                                DummyControllerAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    }
                            , activeClass = "active"
                            , ariaCurrent = "true"
                            , firstName = "Barbara"
                            , lastName = "Bush"
                            }
                        , PersonItem
                            { selectionAction =
                                DummyControllerAction
                                    { selectedPersonId = "20000000-0000-0000-0000-000000000000"
                                    }
                            , activeClass = ""
                            , ariaCurrent = "false"
                            , firstName = "Jackie"
                            , lastName = "Kennedy"
                            }
                        ]
                    }

        it "returns a people column with no one shown as active when no one is selected" do
            let barbara =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "Barbara"
                        |> set #lastName "Bush"

            let jackie =
                    newRecord @Person
                        |> set #id "20000000-0000-0000-0000-000000000000"
                        |> set #firstName "Jackie"
                        |> set #lastName "Kennedy"

            let people = [barbara, jackie]

            buildPeopleNavigation
                DummyControllerAction
                Nothing
                people
                `shouldBe` PeopleNavigation
                    { personItems =
                        [ PersonItem
                            { selectionAction =
                                DummyControllerAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    }
                            , activeClass = ""
                            , ariaCurrent = "false"
                            , firstName = "Barbara"
                            , lastName = "Bush"
                            }
                        , PersonItem
                            { selectionAction =
                                DummyControllerAction
                                    { selectedPersonId = "20000000-0000-0000-0000-000000000000"
                                    }
                            , activeClass = ""
                            , ariaCurrent = "false"
                            , firstName = "Jackie"
                            , lastName = "Kennedy"
                            }
                        ]
                    }

    describe "buildPersonItem" do
        it "returns an inactive person item when not selected" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "Barbara"
                        |> set #lastName "Bush"

            buildPersonItem
                DummyControllerAction
                False
                person
                `shouldBe` PersonItem
                    { selectionAction = DummyControllerAction "10000000-0000-0000-0000-000000000000"
                    , activeClass = ""
                    , ariaCurrent = "false"
                    , firstName = "Barbara"
                    , lastName = "Bush"
                    }

        it "returns an active person item when selected" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "Barbara"
                        |> set #lastName "Bush"

            buildPersonItem
                DummyControllerAction
                True
                person
                `shouldBe` PersonItem
                    { selectionAction = DummyControllerAction "10000000-0000-0000-0000-000000000000"
                    , activeClass = "active"
                    , ariaCurrent = "true"
                    , firstName = "Barbara"
                    , lastName = "Bush"
                    }