module Tests.Application.Timecard.ViewSpec where

import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Timecard.View as Timecard.View
import IHP.Prelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "buildTimecards" do
        it "returns a single timecard when rows are part of the same timecard" do
            let rows =
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "30000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-21"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Just (toTimeOfDay "07:00:00")
                        , timecardEntryClockedOutAt = Just (toTimeOfDay "15:30:00")
                        , timecardEntryLunchDuration = Just 30
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "40000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-22"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Just (toTimeOfDay "07:00:00")
                        , timecardEntryClockedOutAt = Just (toTimeOfDay "15:30:00")
                        , timecardEntryLunchDuration = Just 30
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    ]
            Timecard.View.buildTimecards rows
                `shouldBe` [ Timecard.View.Timecard
                                { id = "10000000-0000-0000-0000-000000000000"
                                , personId = "20000000-0000-0000-0000-000000000000"
                                , weekOf = toDay "2021-06-21"
                                , status = Timecard.View.TimecardInProgress
                                , entries =
                                    [ Timecard.View.TimecardEntry
                                        { id = "30000000-0000-0000-0000-000000000000"
                                        , date = toDay "2021-06-21"
                                        , jobName = "jobName-1"
                                        , clockedInAt = Just $ toTimeOfDay "07:00:00"
                                        , clockedOutAt = Just $ toTimeOfDay "15:30:00"
                                        , lunchDuration = Just 30
                                        , hoursWorked = 8.0
                                        , workDone = "workDone-1"
                                        , invoiceTranslation = "invoiceTranslation-1"
                                        }
                                    , Timecard.View.TimecardEntry
                                        { id = "40000000-0000-0000-0000-000000000000"
                                        , date = toDay "2021-06-22"
                                        , jobName = "jobName-2"
                                        , clockedInAt = Just $ toTimeOfDay "07:00:00"
                                        , clockedOutAt = Just $ toTimeOfDay "15:30:00"
                                        , lunchDuration = Just 30
                                        , hoursWorked = 7.0
                                        , workDone = "workDone-2"
                                        , invoiceTranslation = "invoiceTranslation-2"
                                        }
                                    ]
                                }
                           ]

        it "returns multiple timecards when rows are part of different timecards" do
            let rows =
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "30000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-21"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    , Timecard.Query.Row
                        { timecardId = "40000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-28"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "50000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-29"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    ]
            Timecard.View.buildTimecards rows
                `shouldBe` [ Timecard.View.Timecard
                                { id = "10000000-0000-0000-0000-000000000000"
                                , personId = "20000000-0000-0000-0000-000000000000"
                                , weekOf = toDay "2021-06-21"
                                , status = Timecard.View.TimecardInProgress
                                , entries =
                                    [ Timecard.View.TimecardEntry
                                        { id = "30000000-0000-0000-0000-000000000000"
                                        , date = toDay "2021-06-21"
                                        , jobName = "jobName-1"
                                        , clockedInAt = Nothing
                                        , clockedOutAt = Nothing
                                        , lunchDuration = Nothing
                                        , hoursWorked = 8.0
                                        , workDone = "workDone-1"
                                        , invoiceTranslation = "invoiceTranslation-1"
                                        }
                                    ]
                                }
                           , Timecard.View.Timecard
                                { id = "40000000-0000-0000-0000-000000000000"
                                , personId = "20000000-0000-0000-0000-000000000000"
                                , weekOf = toDay "2021-06-28"
                                , status = Timecard.View.TimecardInProgress
                                , entries =
                                    [ Timecard.View.TimecardEntry
                                        { id = "50000000-0000-0000-0000-000000000000"
                                        , date = toDay "2021-06-29"
                                        , jobName = "jobName-2"
                                        , clockedInAt = Nothing
                                        , clockedOutAt = Nothing
                                        , lunchDuration = Nothing
                                        , hoursWorked = 7.0
                                        , workDone = "workDone-2"
                                        , invoiceTranslation = "invoiceTranslation-2"
                                        }
                                    ]
                                }
                           ]

        it "returns no timecards when there are no rows" do
            Timecard.View.buildTimecards [] `shouldBe` []

        it "returns an in-progress status when some workdays are not accounted for" do
            let rows =
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "30000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-21"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    ]
            Timecard.View.buildTimecards rows
                `shouldBe` [ Timecard.View.Timecard
                                { id = "10000000-0000-0000-0000-000000000000"
                                , personId = "20000000-0000-0000-0000-000000000000"
                                , weekOf = toDay "2021-06-21"
                                , status =
                                    Timecard.View.TimecardInProgress
                                , entries =
                                    [ Timecard.View.TimecardEntry
                                        { id = "30000000-0000-0000-0000-000000000000"
                                        , date = toDay "2021-06-21"
                                        , jobName = "jobName-1"
                                        , clockedInAt = Nothing
                                        , clockedOutAt = Nothing
                                        , lunchDuration = Nothing
                                        , hoursWorked = 8.0
                                        , workDone = "workDone-1"
                                        , invoiceTranslation = "invoiceTranslation-1"
                                        }
                                    ]
                                }
                           ]

        it "returns an in-progress status even when there are multiple rows for the same day" do
            let rows =
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "30000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-21"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "40000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-21"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    ]
            Timecard.View.buildTimecards rows
                `shouldBe` [ Timecard.View.Timecard
                                { id = "10000000-0000-0000-0000-000000000000"
                                , personId = "20000000-0000-0000-0000-000000000000"
                                , weekOf = toDay "2021-06-21"
                                , status = Timecard.View.TimecardInProgress
                                , entries =
                                    [ Timecard.View.TimecardEntry
                                        { id = "30000000-0000-0000-0000-000000000000"
                                        , date = toDay "2021-06-21"
                                        , jobName = "jobName-1"
                                        , clockedInAt = Nothing
                                        , clockedOutAt = Nothing
                                        , lunchDuration = Nothing
                                        , hoursWorked = 8.0
                                        , workDone = "workDone-1"
                                        , invoiceTranslation = "invoiceTranslation-1"
                                        }
                                    , Timecard.View.TimecardEntry
                                        { id = "40000000-0000-0000-0000-000000000000"
                                        , date = toDay "2021-06-21"
                                        , jobName = "jobName-2"
                                        , clockedInAt = Nothing
                                        , clockedOutAt = Nothing
                                        , lunchDuration = Nothing
                                        , hoursWorked = 7.0
                                        , workDone = "workDone-2"
                                        , invoiceTranslation = "invoiceTranslation-2"
                                        }
                                    ]
                                }
                           ]
        it "returns ready for review when all workdays are accounted for" do
            let rows =
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "30000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-21"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "60000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-22"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "90000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-23"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "12000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-24"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "15000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-25"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    ]

            let statuses = get #status <$> Timecard.View.buildTimecards rows
            statuses `shouldBe` [Timecard.View.TimecardReadyForReview]

        it "returns ready for review even when there is a timecard entry on the weekend" do
            let rows =
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "30000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-21"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "60000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-22"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "90000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-23"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "12000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-24"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "15000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-25"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    , Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Nothing
                        , accessTokenValue = Nothing
                        , accessTokenExpiresAt = Nothing
                        , accessTokenIsRevoked = Nothing
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "16000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-26"
                        , timecardEntryJobName = "jobName-2"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 7.0
                        , timecardEntryWorkDone = "workDone-2"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-2"
                        }
                    ]

            let statuses = get #status <$> Timecard.View.buildTimecards rows
            statuses `shouldBe` [Timecard.View.TimecardReadyForReview]

        it "returns under review when there is an access token for the timecard" do
            let rows =
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Just "30000000-0000-0000-0000-000000000000"
                        , accessTokenValue = Just "secret"
                        , accessTokenExpiresAt = Just $ toUtc "2021-06-23 15:29:00 PDT"
                        , accessTokenIsRevoked = Just False
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "40000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-21"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    ]
            Timecard.View.buildTimecards rows
                `shouldBe` [ Timecard.View.Timecard
                                { id = "10000000-0000-0000-0000-000000000000"
                                , personId = "20000000-0000-0000-0000-000000000000"
                                , weekOf = toDay "2021-06-21"
                                , status =
                                    Timecard.View.TimecardUnderReview $
                                        Timecard.View.AccessToken
                                            { id = "30000000-0000-0000-0000-000000000000"
                                            , value = "secret"
                                            , expiresAt = toUtc "2021-06-23 15:29:00 PDT"
                                            , isRevoked = False
                                            }
                                , entries =
                                    [ Timecard.View.TimecardEntry
                                        { id = "40000000-0000-0000-0000-000000000000"
                                        , date = toDay "2021-06-21"
                                        , jobName = "jobName-1"
                                        , clockedInAt = Nothing
                                        , clockedOutAt = Nothing
                                        , lunchDuration = Nothing
                                        , hoursWorked = 8.0
                                        , workDone = "workDone-1"
                                        , invoiceTranslation = "invoiceTranslation-1"
                                        }
                                    ]
                                }
                           ]

        it "returns signed when there is a signing for the timecard" do
            let rows =
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-06-21"
                        , accessTokenId = Just "30000000-0000-0000-0000-000000000000"
                        , accessTokenValue = Just "secret"
                        , accessTokenExpiresAt = Just $ toUtc "2021-06-23 15:29:00 PDT"
                        , accessTokenIsRevoked = Just False
                        , signingId = Just "40000000-0000-0000-0000-000000000000"
                        , signingSignedAt = Just $ toUtc "2021-06-23 15:29:00 PDT"
                        , timecardEntryId = "50000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-06-21"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    ]
            Timecard.View.buildTimecards rows
                `shouldBe` [ Timecard.View.Timecard
                                { id = "10000000-0000-0000-0000-000000000000"
                                , personId = "20000000-0000-0000-0000-000000000000"
                                , weekOf = toDay "2021-06-21"
                                , status =
                                    Timecard.View.TimecardSigned $
                                        Timecard.View.Signing
                                            { id = "40000000-0000-0000-0000-000000000000"
                                            , signedAt = toUtc "2021-06-23 15:29:00 PDT"
                                            }
                                , entries =
                                    [ Timecard.View.TimecardEntry
                                        { id = "50000000-0000-0000-0000-000000000000"
                                        , date = toDay "2021-06-21"
                                        , jobName = "jobName-1"
                                        , clockedInAt = Nothing
                                        , clockedOutAt = Nothing
                                        , lunchDuration = Nothing
                                        , hoursWorked = 8.0
                                        , workDone = "workDone-1"
                                        , invoiceTranslation = "invoiceTranslation-1"
                                        }
                                    ]
                                }
                           ]
