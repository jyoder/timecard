module Tests.Web.View.Timecards.IndexSpec where

import qualified Application.People.View as People.View
import qualified Application.Timecard.View as Timecard.View
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import Test.Hspec
import Tests.Support
import Web.Types
import Web.View.Navigation.People
import qualified Web.View.Timecards.Index as Index
import qualified Web.View.Timecards.Status as Status

spec :: Spec
spec = do
    describe "buildPage" do
        it "returns a timecard page based on the given parameters" do
            let jumpToTop = True

            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "John"
                        , lastName = "Cleese"
                        , goesBy = "John"
                        , state = People.View.PersonIdle
                        }

            let people = [person]

            let personActivity = Index.Viewing

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
                                        TimecardPersonSelectionAction
                                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                            , column = Just "timecards"
                                            , jumpToTop = Just 1
                                            }
                                    , activeClass = ""
                                    , ariaCurrent = "false"
                                    , firstName = "John"
                                    , lastName = "Cleese"
                                    , stateBadge = HiddenBadge
                                    }
                                ]
                            }
                    , timecardsColumnClasses = "d-none d-lg-flex"
                    , timecardsColumn = Index.TimecardsColumnNotVisible
                    , columnNavigation =
                        Index.ColumnNavigation
                            { peopleLinkClass = "text-dark"
                            , peopleAction = TimecardsAction
                            , timecardsLinkClass = "text-muted"
                            , timecardsAction = TimecardsAction
                            }
                    }

    describe "columnClasses" do
        context "when the given column is in view" do
            it "returns classes that allow the column to expand to the whole width of the screen on mobile" do
                Index.columnClasses Index.PeopleColumn Index.PeopleColumn
                    `shouldBe` "d-flex flex-grow-1 flex-lg-grow-0"
        context "when the given column is not in view" do
            it "returns classes that hide the column on mobile" do
                Index.columnClasses Index.PeopleColumn Index.TimecardsColumn
                    `shouldBe` "d-none d-lg-flex"

    describe "buildTimecardColumn" do
        it "returns a non-visible timecard column when no person has been selected" do
            let jumpToTop = True

            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "John"
                        , lastName = "Cleese"
                        , goesBy = "John"
                        , state = People.View.PersonIdle
                        }

            let people = [person]

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecard =
                    Timecard.View.Timecard
                        { id = "30000000-0000-0000-0000-000000000000"
                        , personId = "40000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-06-21"
                        , status = Timecard.View.TimecardInProgress
                        , entries = [timecardEntry]
                        }

            let personActivity = Index.Viewing

            let personSelection = Index.NoPersonSelected

            let currentColumn = Index.TimecardsColumn

            Index.buildTimecardsColumn Index.IndexView {..}
                `shouldBe` Index.TimecardsColumnNotVisible

        it "returns a visible timecard column when a person has been selected" do
            let jumpToTop = True

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

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecard =
                    Timecard.View.Timecard
                        { id = "30000000-0000-0000-0000-000000000000"
                        , personId = "10000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-06-21"
                        , status = Timecard.View.TimecardInProgress
                        , entries = [timecardEntry]
                        }

            let personActivity = Index.Viewing

            let personSelection =
                    Index.PersonSelected
                        { selectedPerson = selectedPerson
                        , timecards = [timecard]
                        , personActivity = personActivity
                        }

            let currentColumn = Index.TimecardsColumn

            Index.buildTimecardsColumn Index.IndexView {..}
                `shouldBe` Index.TimecardsColumnVisible
                    { jumpToTopClass = "scroll-to-pinned"
                    , timecardTables =
                        [ Index.TimecardTable
                            { weekOf = "06/21/2021"
                            , status =
                                Status.TimecardStatus
                                    { statusClasses = "badge badge-pill badge-secondary"
                                    , statusLabel = "In Progress"
                                    }
                            , firstName = "John"
                            , lastName = "Cleese"
                            , jobRows =
                                [ Index.JobRow
                                    { dayOfWeek' = "Wednesday"
                                    , date = "06/23/2021"
                                    , jobNameCell =
                                        Index.ShowCell
                                            { editableField = Index.JobNameField
                                            , value = "job name"
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "jobName"
                                                    }
                                            }
                                    , clockedInAtCell =
                                        Index.ShowCell
                                            { editableField = Index.ClockedInAtField
                                            , value = ""
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "clockedInAt"
                                                    }
                                            }
                                    , clockedOutAtCell =
                                        Index.ShowCell
                                            { editableField = Index.ClockedOutAtField
                                            , value = ""
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "clockedOutAt"
                                                    }
                                            }
                                    , lunchDurationCell =
                                        Index.ShowCell
                                            { editableField = Index.LunchDurationField
                                            , value = ""
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "lunchDuration"
                                                    }
                                            }
                                    , hoursWorkedCell =
                                        Index.ShowCell
                                            { editableField = Index.HoursWorkedField
                                            , value = "5.5"
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "hoursWorked"
                                                    }
                                            }
                                    , workDoneCell =
                                        Index.ShowCell
                                            { editableField = Index.WorkDoneField
                                            , value = "work done"
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "workDone"
                                                    }
                                            }
                                    , invoiceTranslationCell =
                                        Index.ShowCell
                                            { editableField = Index.InvoiceTranslationField
                                            , value = "invoice translation"
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "invoiceTranslation"
                                                    }
                                            }
                                    }
                                ]
                            , totalHoursRow =
                                Index.TotalHoursRow
                                    { totalHours = "5.5"
                                    }
                            , downloadAction =
                                TimecardDownloadTimecardAction
                                    { timecardId = "30000000-0000-0000-0000-000000000000"
                                    }
                            , downloadFileName = "2021-06-21-Cleese-John.pdf"
                            }
                        ]
                    }

        it "does not scroll to the top when the jump to top parameter is false" do
            let jumpToTop = False

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

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecard =
                    Timecard.View.Timecard
                        { id = "30000000-0000-0000-0000-000000000000"
                        , personId = "10000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-06-21"
                        , status = Timecard.View.TimecardInProgress
                        , entries = [timecardEntry]
                        }

            let personActivity = Index.Viewing

            let personSelection =
                    Index.PersonSelected
                        { selectedPerson = selectedPerson
                        , timecards = [timecard]
                        , personActivity = personActivity
                        }

            let currentColumn = Index.TimecardsColumn

            Index.buildTimecardsColumn Index.IndexView {..}
                `shouldBe` Index.TimecardsColumnVisible
                    { jumpToTopClass = ""
                    , timecardTables =
                        [ Index.TimecardTable
                            { weekOf = "06/21/2021"
                            , status =
                                Status.TimecardStatus
                                    { statusClasses = "badge badge-pill badge-secondary"
                                    , statusLabel = "In Progress"
                                    }
                            , firstName = "John"
                            , lastName = "Cleese"
                            , jobRows =
                                [ Index.JobRow
                                    { dayOfWeek' = "Wednesday"
                                    , date = "06/23/2021"
                                    , jobNameCell =
                                        Index.ShowCell
                                            { editableField = Index.JobNameField
                                            , value = "job name"
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "jobName"
                                                    }
                                            }
                                    , clockedInAtCell =
                                        Index.ShowCell
                                            { editableField = Index.ClockedInAtField
                                            , value = ""
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "clockedInAt"
                                                    }
                                            }
                                    , clockedOutAtCell =
                                        Index.ShowCell
                                            { editableField = Index.ClockedOutAtField
                                            , value = ""
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "clockedOutAt"
                                                    }
                                            }
                                    , lunchDurationCell =
                                        Index.ShowCell
                                            { editableField = Index.LunchDurationField
                                            , value = ""
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "lunchDuration"
                                                    }
                                            }
                                    , hoursWorkedCell =
                                        Index.ShowCell
                                            { editableField = Index.HoursWorkedField
                                            , value = "5.5"
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "hoursWorked"
                                                    }
                                            }
                                    , workDoneCell =
                                        Index.ShowCell
                                            { editableField = Index.WorkDoneField
                                            , value = "work done"
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "workDone"
                                                    }
                                            }
                                    , invoiceTranslationCell =
                                        Index.ShowCell
                                            { editableField = Index.InvoiceTranslationField
                                            , value = "invoice translation"
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                                    , editingField = "invoiceTranslation"
                                                    }
                                            }
                                    }
                                ]
                            , totalHoursRow =
                                Index.TotalHoursRow
                                    { totalHours = "5.5"
                                    }
                            , downloadAction =
                                TimecardDownloadTimecardAction
                                    { timecardId = "30000000-0000-0000-0000-000000000000"
                                    }
                            , downloadFileName = "2021-06-21-Cleese-John.pdf"
                            }
                        ]
                    }

    describe "buildTimecardTable" do
        it "returns a timecard table based on the given parameters" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "John"
                        |> set #lastName "Cleese"

            let people = [person]

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Just $ toTimeOfDay "07:30:00"
                        , clockedOutAt = Just $ toTimeOfDay "16:00:00"
                        , lunchDuration = Just 30
                        , hoursWorked = 8.0
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecard =
                    Timecard.View.Timecard
                        { id = "30000000-0000-0000-0000-000000000000"
                        , personId = "40000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-06-21"
                        , status = Timecard.View.TimecardInProgress
                        , entries = [timecardEntry]
                        }

            let personActivity = Index.Viewing

            Index.buildTimecardTable person personActivity timecard
                `shouldBe` Index.TimecardTable
                    { weekOf = "06/21/2021"
                    , status =
                        Status.TimecardStatus
                            { statusClasses = "badge badge-pill badge-secondary"
                            , statusLabel = "In Progress"
                            }
                    , firstName = "John"
                    , lastName = "Cleese"
                    , jobRows =
                        [ Index.JobRow
                            { dayOfWeek' = "Wednesday"
                            , date = "06/23/2021"
                            , jobNameCell =
                                Index.ShowCell
                                    { editableField = Index.JobNameField
                                    , value = "job name"
                                    , editAction =
                                        TimecardEditTimecardEntryAction
                                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                            , editingField = "jobName"
                                            }
                                    }
                            , clockedInAtCell =
                                Index.ShowCell
                                    { editableField = Index.ClockedInAtField
                                    , value = "7:30 AM"
                                    , editAction =
                                        TimecardEditTimecardEntryAction
                                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                            , editingField = "clockedInAt"
                                            }
                                    }
                            , clockedOutAtCell =
                                Index.ShowCell
                                    { editableField = Index.ClockedOutAtField
                                    , value = "4:00 PM"
                                    , editAction =
                                        TimecardEditTimecardEntryAction
                                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                            , editingField = "clockedOutAt"
                                            }
                                    }
                            , lunchDurationCell =
                                Index.ShowCell
                                    { editableField = Index.LunchDurationField
                                    , value = "30"
                                    , editAction =
                                        TimecardEditTimecardEntryAction
                                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                            , editingField = "lunchDuration"
                                            }
                                    }
                            , hoursWorkedCell =
                                Index.ShowCell
                                    { editableField = Index.HoursWorkedField
                                    , value = "8.0"
                                    , editAction =
                                        TimecardEditTimecardEntryAction
                                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                            , editingField = "hoursWorked"
                                            }
                                    }
                            , workDoneCell =
                                Index.ShowCell
                                    { editableField = Index.WorkDoneField
                                    , value = "work done"
                                    , editAction =
                                        TimecardEditTimecardEntryAction
                                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                            , editingField = "workDone"
                                            }
                                    }
                            , invoiceTranslationCell =
                                Index.ShowCell
                                    { editableField = Index.InvoiceTranslationField
                                    , value = "invoice translation"
                                    , editAction =
                                        TimecardEditTimecardEntryAction
                                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                            , editingField = "invoiceTranslation"
                                            }
                                    }
                            }
                        ]
                    , totalHoursRow = Index.TotalHoursRow {totalHours = "8.0"}
                    , downloadAction =
                        TimecardDownloadTimecardAction
                            { timecardId = "30000000-0000-0000-0000-000000000000"
                            }
                    , downloadFileName = "2021-06-21-Cleese-John.pdf"
                    }

    describe "buildJobRow" do
        it "returns a job row based on the given parameters" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "John"
                        |> set #lastName "Cleese"

            let people = [person]

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Just $ toTimeOfDay "15:00:00"
                        , clockedOutAt = Just $ toTimeOfDay "16:00:00"
                        , lunchDuration = Just 30
                        , hoursWorked = 0.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity = Index.Viewing

            Index.buildJobRow person personActivity timecardEntry
                `shouldBe` Index.JobRow
                    { dayOfWeek' = "Wednesday"
                    , date = "06/23/2021"
                    , jobNameCell =
                        Index.ShowCell
                            { editableField = Index.JobNameField
                            , value = "job name"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "jobName"
                                    }
                            }
                    , clockedInAtCell =
                        Index.ShowCell
                            { editableField = Index.ClockedInAtField
                            , value = "3:00 PM"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "clockedInAt"
                                    }
                            }
                    , clockedOutAtCell =
                        Index.ShowCell
                            { editableField = Index.ClockedOutAtField
                            , value = "4:00 PM"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "clockedOutAt"
                                    }
                            }
                    , lunchDurationCell =
                        Index.ShowCell
                            { editableField = Index.LunchDurationField
                            , value = "30"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "lunchDuration"
                                    }
                            }
                    , hoursWorkedCell =
                        Index.ShowCell
                            { editableField = Index.HoursWorkedField
                            , value = "0.5"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "hoursWorked"
                                    }
                            }
                    , workDoneCell =
                        Index.ShowCell
                            { editableField = Index.WorkDoneField
                            , value = "work done"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "workDone"
                                    }
                            }
                    , invoiceTranslationCell =
                        Index.ShowCell
                            { editableField = Index.InvoiceTranslationField
                            , value = "invoice translation"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "invoiceTranslation"
                                    }
                            }
                    }

        it "formats clock in time in 24-hour format when editing" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "John"
                        |> set #lastName "Cleese"

            let people = [person]

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #id "20000000-0000-0000-0000-000000000000"

            let timecardEntry' =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Just $ toTimeOfDay "15:00:00"
                        , clockedOutAt = Just $ toTimeOfDay "16:00:00"
                        , lunchDuration = Just 30
                        , hoursWorked = 0.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity =
                    Index.Editing
                        { selectedTimecardEntry = timecardEntry
                        , editingField = Index.ClockedInAtField
                        }

            Index.buildJobRow person personActivity timecardEntry'
                `shouldBe` Index.JobRow
                    { dayOfWeek' = "Wednesday"
                    , date = "06/23/2021"
                    , jobNameCell =
                        Index.ShowCell
                            { editableField = Index.JobNameField
                            , value = "job name"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "jobName"
                                    }
                            }
                    , clockedInAtCell =
                        Index.EditCell
                            { editableField = Index.ClockedInAtField
                            , value = "15:00:00"
                            , timecardEntryId = "20000000-0000-0000-0000-000000000000"
                            , saveAction =
                                TimecardUpdateTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "clockedInAt"
                                    }
                            , cancelAction =
                                TimecardPersonSelectionAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    , column = Just "timecards"
                                    , jumpToTop = Nothing
                                    }
                            }
                    , clockedOutAtCell =
                        Index.ShowCell
                            { editableField = Index.ClockedOutAtField
                            , value = "4:00 PM"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "clockedOutAt"
                                    }
                            }
                    , lunchDurationCell =
                        Index.ShowCell
                            { editableField = Index.LunchDurationField
                            , value = "30"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "lunchDuration"
                                    }
                            }
                    , hoursWorkedCell =
                        Index.ShowCell
                            { editableField = Index.HoursWorkedField
                            , value = "0.5"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "hoursWorked"
                                    }
                            }
                    , workDoneCell =
                        Index.ShowCell
                            { editableField = Index.WorkDoneField
                            , value = "work done"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "workDone"
                                    }
                            }
                    , invoiceTranslationCell =
                        Index.ShowCell
                            { editableField = Index.InvoiceTranslationField
                            , value = "invoice translation"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "invoiceTranslation"
                                    }
                            }
                    }

        it "formats clock out time in 24-hour format when editing" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "John"
                        |> set #lastName "Cleese"

            let people = [person]

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #id "20000000-0000-0000-0000-000000000000"

            let timecardEntry' =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Just $ toTimeOfDay "15:00:00"
                        , clockedOutAt = Just $ toTimeOfDay "16:00:00"
                        , lunchDuration = Just 30
                        , hoursWorked = 0.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity =
                    Index.Editing
                        { selectedTimecardEntry = timecardEntry
                        , editingField = Index.ClockedOutAtField
                        }

            Index.buildJobRow person personActivity timecardEntry'
                `shouldBe` Index.JobRow
                    { dayOfWeek' = "Wednesday"
                    , date = "06/23/2021"
                    , jobNameCell =
                        Index.ShowCell
                            { editableField = Index.JobNameField
                            , value = "job name"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "jobName"
                                    }
                            }
                    , clockedInAtCell =
                        Index.ShowCell
                            { editableField = Index.ClockedInAtField
                            , value = "3:00 PM"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "clockedInAt"
                                    }
                            }
                    , clockedOutAtCell =
                        Index.EditCell
                            { editableField = Index.ClockedOutAtField
                            , value = "16:00:00"
                            , timecardEntryId = "20000000-0000-0000-0000-000000000000"
                            , saveAction =
                                TimecardUpdateTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "clockedOutAt"
                                    }
                            , cancelAction =
                                TimecardPersonSelectionAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    , column = Just "timecards"
                                    , jumpToTop = Nothing
                                    }
                            }
                    , lunchDurationCell =
                        Index.ShowCell
                            { editableField = Index.LunchDurationField
                            , value = "30"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "lunchDuration"
                                    }
                            }
                    , hoursWorkedCell =
                        Index.ShowCell
                            { editableField = Index.HoursWorkedField
                            , value = "0.5"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "hoursWorked"
                                    }
                            }
                    , workDoneCell =
                        Index.ShowCell
                            { editableField = Index.WorkDoneField
                            , value = "work done"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "workDone"
                                    }
                            }
                    , invoiceTranslationCell =
                        Index.ShowCell
                            { editableField = Index.InvoiceTranslationField
                            , value = "invoice translation"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    , editingField = "invoiceTranslation"
                                    }
                            }
                    }

    describe "buildTableCell" do
        it "returns a non-editable cell when we are not editing the cell" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let people = [person]

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity = Index.Viewing

            Index.buildTableCell
                person
                personActivity
                Index.InvoiceTranslationField
                timecardEntry
                "invoice translation"
                "invoice translation"
                `shouldBe` Index.ShowCell
                    { editableField = Index.InvoiceTranslationField
                    , value = "invoice translation"
                    , editAction =
                        TimecardEditTimecardEntryAction
                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                            , editingField = "invoiceTranslation"
                            }
                    }

        it "returns a non-editable cell when we are editing a different cell" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let people = [person]

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity =
                    Index.Editing
                        { selectedTimecardEntry =
                            newRecord @TimecardEntry
                                |> set #id "30000000-0000-0000-0000-000000000000"
                        , editingField = Index.InvoiceTranslationField
                        }

            Index.buildTableCell
                person
                personActivity
                Index.InvoiceTranslationField
                timecardEntry
                "invoice translation"
                "invoice translation"
                `shouldBe` Index.ShowCell
                    { editableField = Index.InvoiceTranslationField
                    , value = "invoice translation"
                    , editAction =
                        TimecardEditTimecardEntryAction
                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                            , editingField = "invoiceTranslation"
                            }
                    }

        it "returns an editable cell when we are editing the cell" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let people = [person]

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity =
                    Index.Editing
                        { selectedTimecardEntry =
                            newRecord @TimecardEntry
                                |> set #id "20000000-0000-0000-0000-000000000000"
                        , editingField = Index.InvoiceTranslationField
                        }

            Index.buildTableCell
                person
                personActivity
                Index.InvoiceTranslationField
                timecardEntry
                "invoice translation"
                "invoice translation"
                `shouldBe` Index.EditCell
                    { editableField = Index.InvoiceTranslationField
                    , value = "invoice translation"
                    , timecardEntryId = "20000000-0000-0000-0000-000000000000"
                    , saveAction =
                        TimecardUpdateTimecardEntryAction
                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                            , editingField = "invoiceTranslation"
                            }
                    , cancelAction =
                        TimecardPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , column = Just "timecards"
                            , jumpToTop = Nothing
                            }
                    }

    describe "buildTotalHoursRow" do
        it "returns a row with the sum of the hours worked" do
            let timecardEntry1 =
                    Timecard.View.TimecardEntry
                        { id = "10000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecardEntry2 =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 9.3
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let entries = [timecardEntry1, timecardEntry2]

            Index.buildTotalHoursRow entries
                `shouldBe` Index.TotalHoursRow
                    { totalHours = "14.8"
                    }

    describe "buildColumnNavigation" do
        it "uses dark text for the currently selected column" do
            let selectedPerson =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let personSelection =
                    Index.PersonSelected
                        { selectedPerson = selectedPerson
                        , personActivity = Index.Viewing
                        , timecards = []
                        }

            Index.buildColumnNavigation personSelection Index.PeopleColumn
                `shouldBe` Index.ColumnNavigation
                    { peopleLinkClass = "text-dark"
                    , peopleAction =
                        TimecardPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , column = Just "people"
                            , jumpToTop = Just 1
                            }
                    , timecardsLinkClass = "text-muted"
                    , timecardsAction =
                        TimecardPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , column = Just "timecards"
                            , jumpToTop = Just 1
                            }
                    }

    describe "columnToParam" do
        it "returns 'people' for the people column" do
            Index.columnToParam Index.PeopleColumn `shouldBe` "people"
        it "returns 'timecards' for the people column" do
            Index.columnToParam Index.TimecardsColumn `shouldBe` "timecards"

    describe "editableFieldToParam" do
        it "returns 'jobName' for the job name field" do
            Index.editableFieldToParam Index.JobNameField `shouldBe` "jobName"
        it "returns 'clockedInAt' for the clocked in at field" do
            Index.editableFieldToParam Index.ClockedInAtField `shouldBe` "clockedInAt"
        it "returns 'clockedOutAt' for the clocked out at field" do
            Index.editableFieldToParam Index.ClockedOutAtField `shouldBe` "clockedOutAt"
        it "returns 'lunchDuration' for the lunch duration field" do
            Index.editableFieldToParam Index.LunchDurationField `shouldBe` "lunchDuration"
        it "returns 'workDone' for the work done field" do
            Index.editableFieldToParam Index.WorkDoneField `shouldBe` "workDone"
        it "returns 'invoiceTranslation' for the invoice translation field" do
            Index.editableFieldToParam Index.InvoiceTranslationField `shouldBe` "invoiceTranslation"
        it "returns 'hoursWorked' for the hours worked field" do
            Index.editableFieldToParam Index.HoursWorkedField `shouldBe` "hoursWorked"

    describe "editableFieldToClass" do
        it "returns 'job-name' for the job name field" do
            Index.editableFieldToClass Index.JobNameField `shouldBe` "job-name"
        it "returns 'clocked-in-at' for the clocked in at field" do
            Index.editableFieldToClass Index.ClockedInAtField `shouldBe` "clocked-in-at"
        it "returns 'clocked-out-at' for the clocked out at field" do
            Index.editableFieldToClass Index.ClockedOutAtField `shouldBe` "clocked-out-at"
        it "returns 'lunch-duration' for the lunch duration field" do
            Index.editableFieldToClass Index.LunchDurationField `shouldBe` "lunch-duration"
        it "returns 'work-done' for the work done field" do
            Index.editableFieldToClass Index.WorkDoneField `shouldBe` "work-done"
        it "returns 'invoice-translation' for the invoice translation field" do
            Index.editableFieldToClass Index.InvoiceTranslationField `shouldBe` "invoice-translation"
        it "returns 'hours-worked' for the hours worked field" do
            Index.editableFieldToClass Index.HoursWorkedField `shouldBe` "hours-worked"
