module Web.View.Timecards.StatusSpec where

import qualified Application.Timecard.View as Timecard.View
import IHP.Prelude
import Test.Hspec
import Tests.Support
import qualified Web.View.Timecards.Status as Status

spec :: Spec
spec = do
    describe "timecardStatus" do
        it "returns a timecard status based on the given parameters" do
            Status.timecardStatus Timecard.View.TimecardInProgress
                `shouldBe` Status.TimecardStatus
                    { statusClasses = "badge badge-pill badge-secondary"
                    , statusLabel = "In Progress"
                    }

    describe "timecardStatusClasses" do
        it "returns a secondary badge when the status is in progress" do
            Status.timecardStatusClasses Timecard.View.TimecardInProgress
                `shouldBe` "badge badge-pill badge-secondary"

        it "returns a primary badge when the status is ready for review" do
            Status.timecardStatusClasses Timecard.View.TimecardReadyForReview
                `shouldBe` "badge badge-pill badge-primary"

        it "returns a primary badge when the status is under review" do
            Status.timecardStatusClasses
                ( Timecard.View.TimecardUnderReview
                    Timecard.View.AccessToken
                        { id = "10000000-0000-0000-0000-000000000000"
                        , value = "secret"
                        , expiresAt = toUtc "2021-06-23 15:30:00 PDT"
                        , isRevoked = False
                        }
                )
                `shouldBe` "badge badge-pill badge-primary"

        it "returns a success badge when the status is signed" do
            Status.timecardStatusClasses
                ( Timecard.View.TimecardSigned
                    Timecard.View.Signing
                        { id = "10000000-0000-0000-0000-000000000000"
                        , signedAt = toUtc "2021-06-23 15:30:00 PDT"
                        }
                )
                `shouldBe` "badge badge-pill badge-success"

    describe "timecardStatusLabel" do
        it "returns In Progress when the status is in progress" do
            Status.timecardStatusLabel Timecard.View.TimecardInProgress
                `shouldBe` "In Progress"

        it "returns Ready for Review when the status is ready for review" do
            Status.timecardStatusLabel Timecard.View.TimecardReadyForReview
                `shouldBe` "Ready For Review"

        it "returns Under Review when the status is under review" do
            Status.timecardStatusLabel
                ( Timecard.View.TimecardUnderReview
                    Timecard.View.AccessToken
                        { id = "10000000-0000-0000-0000-000000000000"
                        , value = "secret"
                        , expiresAt = toUtc "2021-06-23 15:30:00 PDT"
                        , isRevoked = False
                        }
                )
                `shouldBe` "Under Review"

        it "returns Signed when the status is signed" do
            Status.timecardStatusLabel
                ( Timecard.View.TimecardSigned
                    Timecard.View.Signing
                        { id = "10000000-0000-0000-0000-000000000000"
                        , signedAt = toUtc "2021-06-23 15:30:00 PDT"
                        }
                )
                `shouldBe` "Signed"