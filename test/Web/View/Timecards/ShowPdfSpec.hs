module Web.View.Timecards.ShowPdfSpec where

import qualified Application.Timecard.View as Timecard.View
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import Test.Hspec
import Tests.Support
import qualified Web.View.Timecards.ShowPdf as ShowPdf

spec :: Spec
spec = do
    describe "buildTimecardTable" do
        it "returns a timecard table based on the given parameters" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "John"
                        |> set #lastName "Cleese"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
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

            ShowPdf.buildTimecardTable person timecard
                `shouldBe` ShowPdf.TimecardTable
                    { weekOf = "06/21/2021"
                    , firstName = "John"
                    , lastName = "Cleese"
                    , jobRows =
                        [ ShowPdf.JobRow
                            { dayOfWeek' = "Wednesday"
                            , date = "06/23/2021"
                            , jobName = "job name"
                            , hoursWorked = "5.5"
                            , workDone = "work done"
                            }
                        ]
                    , totalHoursRow =
                        ShowPdf.TotalHoursRow
                            { totalHours = "5.5"
                            }
                    }

    describe "buildJobRow" do
        it "returns a job row based on the given parameters" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "John"
                        |> set #lastName "Cleese"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            ShowPdf.buildJobRow person timecardEntry
                `shouldBe` ShowPdf.JobRow
                    { dayOfWeek' = "Wednesday"
                    , date = "06/23/2021"
                    , jobName = "job name"
                    , hoursWorked = "5.5"
                    , workDone = "work done"
                    }

    describe "buildTotalHoursRow" do
        it "returns a row with the sum of the hours worked" do
            let timecardEntry1 =
                    Timecard.View.TimecardEntry
                        { id = "10000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecardEntry2 =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 9.3
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let entries = [timecardEntry1, timecardEntry2]

            ShowPdf.buildTotalHoursRow entries
                `shouldBe` ShowPdf.TotalHoursRow
                    { totalHours = "14.8"
                    }