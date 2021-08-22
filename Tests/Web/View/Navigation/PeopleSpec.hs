module Tests.Web.View.Navigation.PeopleSpec where

import qualified Application.People.Query as People.Query
import qualified Application.People.View as People.View
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
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            let jackie =
                    People.View.Person
                        { id = "20000000-0000-0000-0000-000000000000"
                        , firstName = "Jackie"
                        , lastName = "Kennedy"
                        , goesBy = "Jackie"
                        , state = People.View.PersonIdle
                        }

            let people = [barbara, jackie]

            let selectedPerson =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            buildPeopleNavigation
                BadgesHidden
                DummyControllerAction
                NoAnchor
                (Just selectedPerson)
                people
                `shouldBe` PeopleNavigation
                    { personItems =
                        [ PersonItem
                            { selectionAction =
                                DummyControllerAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    }
                            , anchor = ""
                            , activeClass = "active"
                            , ariaCurrent = "true"
                            , firstName = "Barbara"
                            , lastName = "Bush"
                            , stateBadge = HiddenBadge
                            }
                        , PersonItem
                            { selectionAction =
                                DummyControllerAction
                                    { selectedPersonId = "20000000-0000-0000-0000-000000000000"
                                    }
                            , anchor = ""
                            , activeClass = ""
                            , ariaCurrent = "false"
                            , firstName = "Jackie"
                            , lastName = "Kennedy"
                            , stateBadge = HiddenBadge
                            }
                        ]
                    }

        it "returns a people column with no one shown as active when no one is selected" do
            let barbara =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            let jackie =
                    People.View.Person
                        { id = "20000000-0000-0000-0000-000000000000"
                        , firstName = "Jackie"
                        , lastName = "Kennedy"
                        , goesBy = "Jackie"
                        , state = People.View.PersonIdle
                        }

            let people = [barbara, jackie]

            buildPeopleNavigation
                BadgesHidden
                DummyControllerAction
                NoAnchor
                Nothing
                people
                `shouldBe` PeopleNavigation
                    { personItems =
                        [ PersonItem
                            { selectionAction =
                                DummyControllerAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    }
                            , anchor = ""
                            , activeClass = ""
                            , ariaCurrent = "false"
                            , firstName = "Barbara"
                            , lastName = "Bush"
                            , stateBadge = HiddenBadge
                            }
                        , PersonItem
                            { selectionAction =
                                DummyControllerAction
                                    { selectedPersonId = "20000000-0000-0000-0000-000000000000"
                                    }
                            , anchor = ""
                            , activeClass = ""
                            , ariaCurrent = "false"
                            , firstName = "Jackie"
                            , lastName = "Kennedy"
                            , stateBadge = HiddenBadge
                            }
                        ]
                    }

        it "returns a people column with visible badges when badges are made visible" do
            let barbara =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            let jackie =
                    People.View.Person
                        { id = "20000000-0000-0000-0000-000000000000"
                        , firstName = "Jackie"
                        , lastName = "Kennedy"
                        , goesBy = "Jackie"
                        , state = People.View.PersonIdle
                        }

            let people = [barbara, jackie]

            buildPeopleNavigation
                BadgesVisible
                DummyControllerAction
                NoAnchor
                Nothing
                people
                `shouldBe` PeopleNavigation
                    { personItems =
                        [ PersonItem
                            { selectionAction =
                                DummyControllerAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    }
                            , anchor = ""
                            , activeClass = ""
                            , ariaCurrent = "false"
                            , firstName = "Barbara"
                            , lastName = "Bush"
                            , stateBadge =
                                VisibleBadge
                                    { label = "Idle"
                                    , classes = "badge badge-pill badge-light"
                                    }
                            }
                        , PersonItem
                            { selectionAction =
                                DummyControllerAction
                                    { selectedPersonId = "20000000-0000-0000-0000-000000000000"
                                    }
                            , anchor = ""
                            , activeClass = ""
                            , ariaCurrent = "false"
                            , firstName = "Jackie"
                            , lastName = "Kennedy"
                            , stateBadge =
                                VisibleBadge
                                    { label = "Idle"
                                    , classes = "badge badge-pill badge-light"
                                    }
                            }
                        ]
                    }

    describe "buildPersonItem" do
        it "returns an inactive person item when not selected" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            buildPersonItem
                BadgesHidden
                DummyControllerAction
                NoAnchor
                False
                person
                `shouldBe` PersonItem
                    { selectionAction = DummyControllerAction "10000000-0000-0000-0000-000000000000"
                    , anchor = ""
                    , activeClass = ""
                    , ariaCurrent = "false"
                    , firstName = "Barbara"
                    , lastName = "Bush"
                    , stateBadge = HiddenBadge
                    }

        it "returns a person item with the correct anchor when one is specified" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            buildPersonItem
                BadgesHidden
                DummyControllerAction
                (Anchor "something")
                False
                person
                `shouldBe` PersonItem
                    { selectionAction = DummyControllerAction "10000000-0000-0000-0000-000000000000"
                    , anchor = "something"
                    , activeClass = ""
                    , ariaCurrent = "false"
                    , firstName = "Barbara"
                    , lastName = "Bush"
                    , stateBadge = HiddenBadge
                    }

        it "returns an active person item when selected" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            buildPersonItem
                BadgesHidden
                DummyControllerAction
                NoAnchor
                True
                person
                `shouldBe` PersonItem
                    { selectionAction =
                        DummyControllerAction
                            "10000000-0000-0000-0000-000000000000"
                    , anchor = ""
                    , activeClass = "active"
                    , ariaCurrent = "true"
                    , firstName = "Barbara"
                    , lastName = "Bush"
                    , stateBadge = HiddenBadge
                    }

        it "returns a person with a badges when badges are visible" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            buildPersonItem
                BadgesVisible
                DummyControllerAction
                NoAnchor
                False
                person
                `shouldBe` PersonItem
                    { selectionAction = DummyControllerAction "10000000-0000-0000-0000-000000000000"
                    , anchor = ""
                    , activeClass = ""
                    , ariaCurrent = "false"
                    , firstName = "Barbara"
                    , lastName = "Bush"
                    , stateBadge =
                        VisibleBadge
                            { label = "Idle"
                            , classes = "badge badge-pill badge-light"
                            }
                    }

    describe "buildStateBadge" do
        it "returns a hidden badge when badges are hidden" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }
            buildStateBadge BadgesHidden person `shouldBe` HiddenBadge

        it "returns a visible badge when badges are visible" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }
            buildStateBadge BadgesVisible person
                `shouldBe` VisibleBadge
                    { label = "Idle"
                    , classes = "badge badge-pill badge-light"
                    }

    describe "personStateLabel" do
        it "returns an idle label when the state is idle" do
            personStateLabel People.View.PersonIdle `shouldBe` "Idle"

        it "returns an auto-pilot label when the state is auto-pilot" do
            personStateLabel People.View.PersonAutoPilot `shouldBe` "Auto Pilot"

        it "returns a needs-attention label when the state is needs-attention" do
            personStateLabel People.View.PersonNeedsAttention `shouldBe` "Needs Attention"

    describe "personStateClasses" do
        it "returns a light pill badge when the state is idle" do
            personStateClasses People.View.PersonIdle `shouldBe` "badge badge-pill badge-light"

        it "returns a light pill badge when the state is auto-pilot" do
            personStateClasses People.View.PersonAutoPilot
                `shouldBe` "badge badge-pill badge-light"

        it "returns a warning pill badge when the state is needs-attention" do
            personStateClasses People.View.PersonNeedsAttention
                `shouldBe` "badge badge-pill badge-warning"
