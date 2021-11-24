module Tests.Application.Timecard.ReviewRequestSpec where

import qualified Application.Base.WorkerSettings as WorkerSettings
import qualified Application.Timecard.ReviewRequest as ReviewRequest
import Generated.Types
import IHP.ControllerPrelude
import IHP.Environment
import Test.Hspec
import Tests.Support
import Text.Read (read)

spec :: Spec
spec = do
    describe "requestTime" do
        it "returns a time 4 minutes after the given time" do
            ReviewRequest.requestTime (toUtc "2021-09-20 00:00:00 PDT")
                `shouldBe` toUtc "2021-09-20 00:04:00 PDT"

    describe "requestBody" do
        it "returns the body of the timecard review request in English when the preferred language is English" do
            ReviewRequest.requestBody WorkerSettings.English "Laura"
                `shouldBe` "Thanks Laura. Here's your timecard to review and sign. Let me know if you need me to make any corrections on it:"
        it "returns the body of the timecard review request in Spanish when the preferred language is Spanish" do
            ReviewRequest.requestBody WorkerSettings.Spanish "Laura"
                `shouldBe` "Gracias Laura. Aqu\237 est\225 su tarjeta de tiempo para revisar y firmar. Av\237seme si necesita que le haga alguna correcci\243n:"

    describe "linkTime" do
        it "returns 4 minutes and 10 seconds after the given time" do
            ReviewRequest.linkTime (toUtc "2021-09-20 00:00:00 PDT")
                `shouldBe` toUtc "2021-09-20 00:04:10 PDT"

    describe "reviewLink" do
        it "returns the full URL for a timecard review" do
            ReviewRequest.reviewLink "https://timecard.company.com" "secrettoken"
                `shouldBe` "https://timecard.company.com/ShowTimecardReview?accessToken=secrettoken"

    aroundAll (withApp RootApplication testConfig) do
        describe "scheduleRequest" do
            itIO "schedules a request for a timecard review" do
                user <-
                    newRecord @User
                        |> set #email "test@user.com"
                        |> set #passwordHash "some password hash"
                        |> createRecord

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

                newRecord @WorkerSetting
                    |> set #personId (get #id worker)
                    |> set #sendDailyReminderAt (toTimeOfDay "15:30:00")
                    |> set #preferredLanguage WorkerSettings.english
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

                (requestMessage, linkMessage) <-
                    ReviewRequest.scheduleRequest
                        (Just $ get #id user)
                        "https://fake.com"
                        (toUtc "2021-09-17 15:30:00 PDT")
                        (get #id timecard)
                        worker
                        (get #id botPhoneNumber)
                        (get #id workerPhoneNumber)

                timecardAccessToken <-
                    query @TimecardAccessToken
                        |> filterWhere (#timecardId, get #id timecard)
                        |> fetchOne
                accessToken <- fetch $ get #accessTokenId timecardAccessToken
                let accessTokenValue = get #value accessToken

                get #fromId requestMessage `shouldBe` get #id botPhoneNumber
                get #toId requestMessage `shouldBe` get #id workerPhoneNumber
                get #body requestMessage
                    `shouldBe` "Thanks Laura. Here's your timecard to review and sign. Let me know if you need me to make any corrections on it:"

                get #fromId linkMessage `shouldBe` get #id botPhoneNumber
                get #toId linkMessage `shouldBe` get #id workerPhoneNumber
                get #body linkMessage
                    `shouldBe` "https://fake.com/ShowTimecardReview?accessToken=" <> accessTokenValue

                reviewLinkGeneratedCount <-
                    query @AuditEntry
                        |> filterWhere (#userId, Just $ get #id user)
                        |> filterWhere (#phoneNumberId, get #id workerPhoneNumber)
                        |> filterWhere (#action, ReviewLinkGenerated)
                        |> fetchCount
                reviewLinkGeneratedCount `shouldBe` 1

                reviewRequestScheduledCount <-
                    query @AuditEntry
                        |> filterWhere (#userId, Nothing)
                        |> filterWhere (#phoneNumberId, get #id workerPhoneNumber)
                        |> filterWhere (#action, ReviewRequestScheduled)
                        |> fetchCount
                reviewRequestScheduledCount `shouldBe` 1

            itIO "schedules a request in Spanish if the worker prefers Spanish" do
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

                newRecord @WorkerSetting
                    |> set #personId (get #id worker)
                    |> set #sendDailyReminderAt (toTimeOfDay "15:30:00")
                    |> set #preferredLanguage WorkerSettings.spanish
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

                (requestMessage, linkMessage) <-
                    ReviewRequest.scheduleRequest
                        Nothing
                        "https://fake.com"
                        (toUtc "2021-09-17 15:30:00 PDT")
                        (get #id timecard)
                        worker
                        (get #id botPhoneNumber)
                        (get #id workerPhoneNumber)

                timecardAccessToken <- query @TimecardAccessToken |> filterWhere (#timecardId, get #id timecard) |> fetchOne
                accessToken <- fetch $ get #accessTokenId timecardAccessToken
                let accessTokenValue = get #value accessToken

                get #fromId requestMessage `shouldBe` get #id botPhoneNumber
                get #toId requestMessage `shouldBe` get #id workerPhoneNumber
                get #body requestMessage
                    `shouldBe` "Gracias Laura. Aqu\237 est\225 su tarjeta de tiempo para revisar y firmar. Av\237seme si necesita que le haga alguna correcci\243n:"

                get #fromId linkMessage `shouldBe` get #id botPhoneNumber
                get #toId linkMessage `shouldBe` get #id workerPhoneNumber
                get #body linkMessage
                    `shouldBe` "https://fake.com/ShowTimecardReview?accessToken=" <> accessTokenValue