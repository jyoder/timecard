module Tests.Application.Timecard.ReviewRequestSpec where

import qualified Application.Timecard.ReviewRequest as ReviewRequest
import Generated.Types
import IHP.ControllerPrelude
import IHP.Environment
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support
import Text.Read (read)

spec :: Spec
spec = do
    describe "reviewLink" do
        it "returns the full URL for a timecard review" do
            ReviewRequest.reviewLink "https://timecard.company.com" "secrettoken"
                `shouldBe` "https://timecard.company.com/ShowTimecardReview?accessToken=secrettoken"

    describe "requestTime" do
        it "returns a time 4 minutes after the given time" do
            ReviewRequest.requestTime (toUtc "2021-09-20 00:00:00 PDT")
                `shouldBe` toUtc "2021-09-20 00:04:00 PDT"

    describe "requestBody" do
        it "returns the body of the timecard review request" do
            ReviewRequest.requestBody "Laura" "https://fake.com"
                `shouldBe` "Thanks Laura. Here's your timecard to review and sign:\nhttps://fake.com\n\nLet me know if you need me to make any corrections on it. Have a good weekend!"

    describe "scheduleRequest" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "schedules a request for a timecard review" do
                bot <-
                    newRecord @Person
                        |> set #firstName "Tim"
                        |> set #lastName "Eckard"
                        |> set #goesBy "Tim the Bot"
                        |> createRecord

                botPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id bot)
                    |> set #phoneNumberId (get #id botPhoneNumber)
                    |> createRecord

                worker <-
                    newRecord @Person
                        |> set #firstName "Laura"
                        |> set #lastName "Wilder"
                        |> set #goesBy "Laura"
                        |> createRecord

                workerPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+16666666666"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id worker)
                    |> set #phoneNumberId (get #id workerPhoneNumber)
                    |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #personId (get #id worker)
                        |> set #weekOf (toDay "2021-09-13")
                        |> createRecord

                sendMessageAction <-
                    ReviewRequest.scheduleRequest
                        "https://fake.com"
                        (toUtc "2021-09-17 15:30:00 PDT")
                        (get #id timecard)
                        "Mrs. Laura"
                        (get #id botPhoneNumber)
                        (get #id workerPhoneNumber)

                timecardAccessToken <- query @TimecardAccessToken |> filterWhere (#timecardId, get #id timecard) |> fetchOne
                accessToken <- fetch $ get #accessTokenId timecardAccessToken
                let accessTokenValue = get #value accessToken

                get #fromId sendMessageAction `shouldBe` get #id botPhoneNumber
                get #toId sendMessageAction `shouldBe` get #id workerPhoneNumber
                get #body sendMessageAction
                    `shouldBe` "Thanks Mrs. Laura. Here's your timecard to review and sign:\nhttps://fake.com/ShowTimecardReview?accessToken="
                    <> accessTokenValue
                    <> "\n\nLet me know if you need me to make any corrections on it. Have a good weekend!"