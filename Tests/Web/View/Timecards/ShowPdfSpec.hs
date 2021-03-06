module Tests.Web.View.Timecards.ShowPdfSpec where

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
                            , clockedInAt = "--"
                            , clockedOutAt = "--"
                            , lunchDuration = "--"
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
                        , clockedInAt = Just $ toTimeOfDay "07:30:00"
                        , clockedOutAt = Just $ toTimeOfDay "16:00:00"
                        , lunchDuration = Just 30
                        , hoursWorked = 8.0
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            ShowPdf.buildJobRow person timecardEntry
                `shouldBe` ShowPdf.JobRow
                    { dayOfWeek' = "Wednesday"
                    , date = "06/23/2021"
                    , jobName = "job name"
                    , clockedInAt = "7:30 AM"
                    , clockedOutAt = "4:00 PM"
                    , lunchDuration = "30"
                    , hoursWorked = "8.0"
                    , workDone = "work done"
                    }

        it "uses dashes to represent blank fields" do
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
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            ShowPdf.buildJobRow person timecardEntry
                `shouldBe` ShowPdf.JobRow
                    { dayOfWeek' = "Wednesday"
                    , date = "06/23/2021"
                    , jobName = "job name"
                    , clockedInAt = "--"
                    , clockedOutAt = "--"
                    , lunchDuration = "--"
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

            ShowPdf.buildTotalHoursRow entries
                `shouldBe` ShowPdf.TotalHoursRow
                    { totalHours = "14.8"
                    }

    describe "buildSignatureBlock" do
        it "returns a signed block when a signing is present" do
            let signing =
                    newRecord @Signing
                        |> set #name "Ronald McDonald"
                        |> set #signedAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> set #ipAddress "127.0.0.1"

            ShowPdf.buildSignatureBlock (Just signing)
                `shouldBe` ShowPdf.SignedBlock
                    { name = "Ronald McDonald"
                    , signedAt = "2021-06-23 22:00:00 UTC"
                    , ipAddress = "127.0.0.1"
                    }

        it "returns a not signed block when no signing is present" do
            ShowPdf.buildSignatureBlock Nothing `shouldBe` ShowPdf.NotSignedBlock