module Tests.Web.View.TimecardReviews.ShowSpec where

import qualified Application.Timecard.View as V
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude
import Test.Hspec
import Tests.Support
import Web.Types
import Web.View.TimecardReviews.Show

spec :: Spec
spec = do
    describe "buildReviewStatus" do
        it "returns a review found status when the review was found" do
            let viewAccessToken =
                    V.AccessToken
                        { id = "10000000-0000-0000-0000-000000000000"
                        , value = "1234"
                        , expiresAt = toUtc "2021-08-14 10:30:05 UTC"
                        , isRevoked = False
                        }

            let entries =
                    [ V.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-08-14"
                        , jobName = "Christopher Lloyd's house"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 8.3
                        , workDone = "stuff"
                        , invoiceTranslation = "stuff"
                        }
                    , V.TimecardEntry
                        { id = "30000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-08-15"
                        , jobName = "Tim Allen's house"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 4.5
                        , workDone = "stuff"
                        , invoiceTranslation = "stuff"
                        }
                    ]

            let timecard =
                    V.Timecard
                        { id = "40000000-0000-0000-0000-000000000000"
                        , personId = "50000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-08-14"
                        , status = V.TimecardUnderReview viewAccessToken
                        , entries
                        }

            let accessToken =
                    newRecord @AccessToken
                        |> set #id "60000000-0000-0000-0000-000000000000"
                        |> set #value "1234"

            let signing =
                    newRecord @Signing
                        |> set #id "70000000-0000-0000-0000-000000000000"
                        |> set #name "first last"
                        |> set #signedAt (toUtc "2021-08-14 10:30:05 UTC")
                        |> set #ipAddress "192.168.1.1"

            let person =
                    newRecord @Person
                        |> set #id "80000000-0000-0000-0000-000000000000"
                        |> set #firstName "first"
                        |> set #lastName "last"

            let review =
                    ReviewFound
                        { person
                        , timecard
                        , accessToken
                        , signing
                        }

            buildReviewStatus review
                `shouldBe` ReviewFoundStatus
                    { firstName = "first"
                    , lastName = "last"
                    , weekOf = "08/14/2021"
                    , timecardEntryCards =
                        [ TimecardEntryCard
                            { day = "Saturday"
                            , date = "08/14/2021"
                            , hoursWorked = "8.3"
                            , jobName = "Christopher Lloyd's house"
                            , clockDetails = NoClockDetails
                            , workDone = "stuff"
                            }
                        , TimecardEntryCard
                            { day = "Sunday"
                            , date = "08/15/2021"
                            , hoursWorked = "4.5"
                            , jobName = "Tim Allen's house"
                            , clockDetails = NoClockDetails
                            , workDone = "stuff"
                            }
                        ]
                    , totalHoursCard =
                        TotalHoursCard
                            { totalHours = "12.8"
                            }
                    , signatureBlock =
                        Signed
                            { completedSignature =
                                CompletedSignature
                                    { signedBy = "first last"
                                    , signedAt = "2021-08-14 10:30:05 UTC"
                                    , ipAddress = "192.168.1.1"
                                    }
                            }
                    , fullStoryScript =
                        FullStoryScript
                            { personId = "80000000-0000-0000-0000-000000000000"
                            , displayName = "first last"
                            }
                    }

        it "returns a review not found status when the review was not found" do
            buildReviewStatus ReviewNotFound `shouldBe` ReviewNotFoundStatus

        it "returns a review expired when the review was expired" do
            buildReviewStatus ReviewExpired `shouldBe` ReviewExpiredStatus

    describe "buildTimecardEntryCard" do
        it "returns a timecard entry card based on the given parameters" do
            let timecardEntry =
                    V.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-08-14"
                        , jobName = "Christopher Lloyd's house"
                        , clockedInAt = Just $ toTimeOfDay "07:30:00"
                        , clockedOutAt = Just $ toTimeOfDay "15:30:00"
                        , lunchDuration = Just 30
                        , hoursWorked = 8.3
                        , workDone = "stuff"
                        , invoiceTranslation = "stuff"
                        }

            buildTimecardEntryCard timecardEntry
                `shouldBe` TimecardEntryCard
                    { day = "Saturday"
                    , date = "08/14/2021"
                    , hoursWorked = "8.3"
                    , jobName = "Christopher Lloyd's house"
                    , clockDetails =
                        ClockDetails
                            { clockedInAt = "7:30 AM"
                            , clockedOutAt = "3:30 PM"
                            , lunchDuration = "30"
                            }
                    , workDone = "stuff"
                    }

    describe "buildClockDetails" do
        it "returns clock details if all parameters are available" do
            let accessToken =
                    V.AccessToken
                        { id = "10000000-0000-0000-0000-000000000000"
                        , value = "1234"
                        , expiresAt = toUtc "2021-08-14 10:30:05 UTC"
                        , isRevoked = False
                        }

            let timecardEntry =
                    V.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-08-14"
                        , jobName = "Christopher Lloyd's house"
                        , clockedInAt = Just $ toTimeOfDay "07:30:00"
                        , clockedOutAt = Just $ toTimeOfDay "15:30:00"
                        , lunchDuration = Just 30
                        , hoursWorked = 8.3
                        , workDone = "stuff"
                        , invoiceTranslation = "stuff"
                        }

            buildClockDetails timecardEntry
                `shouldBe` ClockDetails
                    { clockedInAt = "7:30 AM"
                    , clockedOutAt = "3:30 PM"
                    , lunchDuration = "30"
                    }

        it "returns no clock details if a clock parameter is missing" do
            let accessToken =
                    V.AccessToken
                        { id = "10000000-0000-0000-0000-000000000000"
                        , value = "1234"
                        , expiresAt = toUtc "2021-08-14 10:30:05 UTC"
                        , isRevoked = False
                        }

            let timecardEntry =
                    V.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-08-14"
                        , jobName = "Christopher Lloyd's house"
                        , clockedInAt = Just $ toTimeOfDay "07:30:00"
                        , clockedOutAt = Nothing
                        , lunchDuration = Just 30
                        , hoursWorked = 8.3
                        , workDone = "stuff"
                        , invoiceTranslation = "stuff"
                        }

            buildClockDetails timecardEntry `shouldBe` NoClockDetails

    describe "buildTotalHoursCard" do
        it "returns a total hours card with the sum of timecard entry hours" do
            let accessToken =
                    V.AccessToken
                        { id = "10000000-0000-0000-0000-000000000000"
                        , value = "1234"
                        , expiresAt = toUtc "2021-08-14 10:30:05 UTC"
                        , isRevoked = False
                        }

            let entries =
                    [ V.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-08-14"
                        , jobName = "Christopher Lloyd's house"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 8.3
                        , workDone = "stuff"
                        , invoiceTranslation = "stuff"
                        }
                    , V.TimecardEntry
                        { id = "30000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-08-15"
                        , jobName = "Tim Allen's house"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 4.5
                        , workDone = "stuff"
                        , invoiceTranslation = "stuff"
                        }
                    ]

            let timecard =
                    V.Timecard
                        { id = "40000000-0000-0000-0000-000000000000"
                        , personId = "50000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-08-14"
                        , status = V.TimecardUnderReview accessToken
                        , entries
                        }

            buildTotalHoursCard timecard
                `shouldBe` TotalHoursCard
                    { totalHours = "12.8"
                    }

    describe "buildSignatureBlock" do
        it "returns an unsigned signature form if the signature is new" do
            let accessToken =
                    newRecord @AccessToken
                        |> set #value "1234"

            let signing = newRecord @Signing

            buildSignatureBlock accessToken signing
                `shouldBe` NotSigned
                    { signatureForm =
                        SignatureForm
                            { signing
                            , signAction = CreateSigningAction
                            , accessToken = "1234"
                            }
                    }

        it "returns a completed signature form if the signature is not new" do
            let accessToken =
                    newRecord @AccessToken
                        |> set #value "1234"

            let signing =
                    newRecord @Signing
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #name "Bon Jovi"
                        |> set #signedAt (toUtc "2021-08-14 10:30:05 UTC")
                        |> set #ipAddress "192.168.1.1"

            buildSignatureBlock accessToken signing
                `shouldBe` Signed
                    { completedSignature =
                        CompletedSignature
                            { signedBy = "Bon Jovi"
                            , signedAt = "2021-08-14 10:30:05 UTC"
                            , ipAddress = "192.168.1.1"
                            }
                    }

    describe "buildSignatureForm" do
        it "returns a signature form based on the given parameters" do
            let accessToken =
                    newRecord @AccessToken
                        |> set #value "1234"

            let signing =
                    newRecord @Signing
                        |> set #id "10000000-0000-0000-0000-000000000000"

            buildSignatureForm accessToken signing
                `shouldBe` SignatureForm
                    { signing
                    , signAction = CreateSigningAction
                    , accessToken = "1234"
                    }

    describe "buildCompletedSignature" do
        it "returns a completed signature based on the given parameters" do
            let signing =
                    newRecord @Signing
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #name "Jack Sparrow"
                        |> set #signedAt (toUtc "2021-08-14 10:30:05 UTC")
                        |> set #ipAddress "192.168.1.1"

            buildCompletedSignature signing
                `shouldBe` CompletedSignature
                    { signedBy = "Jack Sparrow"
                    , signedAt = "2021-08-14 10:30:05 UTC"
                    , ipAddress = "192.168.1.1"
                    }

    describe "buildFullStoryScript" do
        it "returns a full story script based on the given parameters" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "Ralph"
                        |> set #lastName "Nader"

            buildFullStoryScript person
                `shouldBe` FullStoryScript
                    { personId = "10000000-0000-0000-0000-000000000000"
                    , displayName = "Ralph Nader"
                    }
