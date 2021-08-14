module Web.View.Timecards.IndexSpec where

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

            Index.buildPage Index.IndexView {..}
                `shouldBe` Index.Page
                    { selectedPerson = Nothing
                    , peopleNavigation =
                        PeopleNavigation
                            { personItems =
                                [ PersonItem
                                    { selectionAction =
                                        TimecardPersonSelectionAction
                                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                            }
                                    , anchor = "timecards"
                                    , activeClass = ""
                                    , ariaCurrent = "false"
                                    , firstName = "John"
                                    , lastName = "Cleese"
                                    , stateBadge = HiddenBadge
                                    }
                                ]
                            }
                    , timecardColumn = Index.TimecardColumnNotVisible
                    }

    describe "buildTimecardColumn" do
        it "returns a non-visible timecard column when no person has been selected" do
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

            Index.buildTimecardColumn Index.IndexView {..}
                `shouldBe` Index.TimecardColumnNotVisible

        it "returns a visible timecard column when a person has been selected" do
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

            Index.buildTimecardColumn Index.IndexView {..}
                `shouldBe` Index.TimecardColumnVisible
                    { timecardTables =
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
                                    , jobName = "job name"
                                    , clockedInAt = "--"
                                    , clockedOutAt = "--"
                                    , lunchDuration = "--"
                                    , hoursWorked = "5.5"
                                    , workDone = "work done"
                                    , invoiceTranslationCell =
                                        Index.ShowInvoiceTranslation
                                            { invoiceTranslation = "invoice translation"
                                            , editAction =
                                                TimecardEditTimecardEntryAction
                                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
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
                            , jobName = "job name"
                            , clockedInAt = "7:30 AM"
                            , clockedOutAt = "4:00 PM"
                            , lunchDuration = "30"
                            , hoursWorked = "8.0"
                            , workDone = "work done"
                            , invoiceTranslationCell =
                                Index.ShowInvoiceTranslation
                                    { invoiceTranslation = "invoice translation"
                                    , editAction =
                                        TimecardEditTimecardEntryAction
                                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
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

        it "uses dashes to represent blank fields" do
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

            Index.buildTimecardTable person personActivity timecard
                `shouldBe` Index.TimecardTable
                    { weekOf = "06/21/2021"
                    , status = Status.TimecardStatus {statusClasses = "badge badge-pill badge-secondary", statusLabel = "In Progress"}
                    , firstName = "John"
                    , lastName = "Cleese"
                    , jobRows =
                        [ Index.JobRow
                            { dayOfWeek' = "Wednesday"
                            , date = "06/23/2021"
                            , jobName = "job name"
                            , clockedInAt = "--"
                            , clockedOutAt = "--"
                            , lunchDuration = "--"
                            , hoursWorked = "5.5"
                            , workDone = "work done"
                            , invoiceTranslationCell =
                                Index.ShowInvoiceTranslation
                                    { invoiceTranslation = "invoice translation"
                                    , editAction =
                                        TimecardEditTimecardEntryAction
                                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                            }
                                    }
                            }
                        ]
                    , totalHoursRow = Index.TotalHoursRow {totalHours = "5.5"}
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
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity = Index.Viewing

            Index.buildJobRow person personActivity timecardEntry
                `shouldBe` Index.JobRow
                    { dayOfWeek' = "Wednesday"
                    , date = "06/23/2021"
                    , jobName = "job name"
                    , clockedInAt = "--"
                    , clockedOutAt = "--"
                    , lunchDuration = "--"
                    , hoursWorked = "5.5"
                    , workDone = "work done"
                    , invoiceTranslationCell =
                        Index.ShowInvoiceTranslation
                            { invoiceTranslation = "invoice translation"
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    }
                            }
                    }

    describe "buildInvoiceTranslationCell" do
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

            Index.buildInvoiceTranslationCell person personActivity timecardEntry
                `shouldBe` Index.ShowInvoiceTranslation
                    { invoiceTranslation = "invoice translation"
                    , editAction =
                        TimecardEditTimecardEntryAction
                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
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
                    Index.EditingInvoiceTranslation
                        { selectedTimecardEntry =
                            newRecord @TimecardEntry
                                |> set #id "30000000-0000-0000-0000-000000000000"
                        }

            Index.buildInvoiceTranslationCell person personActivity timecardEntry
                `shouldBe` Index.ShowInvoiceTranslation
                    { invoiceTranslation = "invoice translation"
                    , editAction =
                        TimecardEditTimecardEntryAction
                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
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
                    Index.EditingInvoiceTranslation
                        { selectedTimecardEntry =
                            newRecord @TimecardEntry
                                |> set #id "20000000-0000-0000-0000-000000000000"
                        }

            Index.buildInvoiceTranslationCell person personActivity timecardEntry
                `shouldBe` Index.EditInvoiceTranslation
                    { invoiceTranslation = "invoice translation"
                    , timecardEntryId = "20000000-0000-0000-0000-000000000000"
                    , saveAction =
                        TimecardUpdateTimecardEntryAction
                            { timecardEntryId = "20000000-0000-0000-0000-000000000000"
                            }
                    , cancelAction =
                        TimecardPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
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
