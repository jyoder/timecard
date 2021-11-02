module Tests.Web.View.AuditEntries.IndexSpec where

import qualified Application.Audit.Query as Audit.Query
import qualified Application.People.View as People.View
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import Test.Hspec
import Tests.Support
import Web.Types
import qualified Web.View.AuditEntries.Index as Index
import Web.View.Navigation.People

spec :: Spec
spec = do
    describe "buildPage" do
        it "returns a timecard page based on the given parameters" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "John"
                        , lastName = "Cleese"
                        , goesBy = "John"
                        , state = People.View.PersonIdle
                        }

            let people = [person]

            let personSelection = Index.NoPersonSelected

            let currentColumn = Index.PeopleColumn

            Index.buildPage Index.IndexView {..}
                `shouldBe` Index.Page
                    { selectedPerson = Nothing
                    , peopleNavigationClasses = "d-flex flex-grow-1 flex-lg-grow-0"
                    , peopleNavigation =
                        PeopleNavigation
                            { personItems =
                                [ PersonItem
                                    { selectionAction =
                                        AuditEntriesPersonSelectionAction
                                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                            , column = Just "entries"
                                            }
                                    , activeClass = ""
                                    , ariaCurrent = "false"
                                    , firstName = "John"
                                    , lastName = "Cleese"
                                    , stateBadge = HiddenBadge
                                    }
                                ]
                            }
                    , auditColumnClasses = "d-none d-lg-flex"
                    , auditColumn = Index.AuditColumnNotVisible
                    , columnNavigation =
                        Index.ColumnNavigation
                            { peopleLinkClass = "text-dark"
                            , peopleAction = AuditEntriesAction
                            , auditLinkClass = "text-muted"
                            , auditAction = AuditEntriesAction
                            }
                    }

    describe "columnClasses" do
        context "when the given column is in view" do
            it "returns classes that allow the column to expand to the whole width of the screen on mobile" do
                Index.columnClasses Index.PeopleColumn Index.PeopleColumn
                    `shouldBe` "d-flex flex-grow-1 flex-lg-grow-0"
        context "when the given column is not in view" do
            it "returns classes that hide the column on mobile" do
                Index.columnClasses Index.PeopleColumn Index.EntriesColumn
                    `shouldBe` "d-none d-lg-flex"

    describe "buildAuditColumn" do
        it "returns a non-visible audit column when no one is selected" do
            Index.buildAuditColumn
                Index.IndexView
                    { people = []
                    , personSelection = Index.NoPersonSelected
                    , currentColumn = Index.EntriesColumn
                    }
                `shouldBe` Index.AuditColumnNotVisible

        it "returns a non-visible audit column when someone is selected but there are no entries" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "John"
                        , lastName = "Cleese"
                        , goesBy = "John"
                        , state = People.View.PersonIdle
                        }

            let people = [person]

            let selectedPerson =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "John"
                        |> set #lastName "Cleese"

            let auditEntries = []

            let personSelection = Index.PersonSelected {..}

            Index.buildAuditColumn
                Index.IndexView
                    { people
                    , personSelection = Index.PersonSelected {..}
                    , currentColumn = Index.EntriesColumn
                    }
                `shouldBe` Index.AuditColumnNotVisible

        it "returns a visible audit column when someone is selected and there are audit entries" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "John"
                        , lastName = "Cleese"
                        , goesBy = "John"
                        , state = People.View.PersonIdle
                        }

            let people = [person]

            let selectedPerson =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "John"
                        |> set #lastName "Cleese"

            let auditEntries =
                    [ Audit.Query.Row
                        { createdAt = toUtc "2021-10-30 07:00:00 PDT"
                        , createdBy = "john@cleese.com"
                        , action = MessageSent
                        , actionContext = "I sent this message."
                        }
                    ]

            let personSelection = Index.PersonSelected {..}

            Index.buildAuditColumn
                Index.IndexView
                    { people
                    , personSelection = Index.PersonSelected {..}
                    , currentColumn = Index.EntriesColumn
                    }
                `shouldBe` Index.AuditColumnVisible
                    { auditEntriesTable =
                        Index.AuditEntriesTable
                            { auditEntryRows =
                                [ Index.AuditEntryRow
                                    { createdAt = "2021-10-30T14:00:00+0000"
                                    , createdBy = "john@cleese.com"
                                    , action = "MessageSent"
                                    , actionContext = "I sent this message."
                                    }
                                ]
                            }
                    }

    describe "buildAuditEntryRow" do
        it "returns an audit entry row based on the given parameters" do
            Index.buildAuditEntryRow
                Audit.Query.Row
                    { createdAt = toUtc "2021-10-30 07:00:00 PDT"
                    , createdBy = Just "test@company.com"
                    , action = MessageSent
                    , actionContext = "Barfy."
                    }
                `shouldBe` Index.AuditEntryRow
                    { createdAt = "2021-10-30T14:00:00+0000"
                    , createdBy = "test@company.com"
                    , action = "MessageSent"
                    , actionContext = "Barfy."
                    }

        it "uses 'System' when createdBy is nothing" do
            Index.buildAuditEntryRow
                Audit.Query.Row
                    { createdAt = toUtc "2021-10-30 07:00:00 PDT"
                    , createdBy = Nothing
                    , action = MessageSent
                    , actionContext = "Barfy."
                    }
                `shouldBe` Index.AuditEntryRow
                    { createdAt = "2021-10-30T14:00:00+0000"
                    , createdBy = "System"
                    , action = "MessageSent"
                    , actionContext = "Barfy."
                    }

    describe "buildColumnNavigation" do
        it "uses dark text for the currently selected column" do
            let selectedPerson =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let personSelection =
                    Index.PersonSelected
                        { selectedPerson = selectedPerson
                        , auditEntries = []
                        }

            Index.buildColumnNavigation personSelection Index.PeopleColumn
                `shouldBe` Index.ColumnNavigation
                    { peopleLinkClass = "text-dark"
                    , peopleAction =
                        AuditEntriesPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , column = Just "people"
                            }
                    , auditLinkClass = "text-muted"
                    , auditAction =
                        AuditEntriesPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , column = Just "entries"
                            }
                    }

    describe "columnToParam" do
        it "returns 'people' when the input is PeopleColumn" do
            Index.columnToParam Index.PeopleColumn `shouldBe` "people"

        it "returns 'entries' when the input is EntriesColumn" do
            Index.columnToParam Index.EntriesColumn `shouldBe` "entries"