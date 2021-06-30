module Application.Timecard.TimecardEntryRequestSpec where

import Application.Timecard.TimecardEntryRequest
import Generated.Types
import IHP.ControllerPrelude
import IHP.Environment
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support
import Text.Read (read)

spec :: Spec
spec = do
    describe "nextRequestTime" $ do
        describe "Monday" $ do
            it "returns a request time for Monday if the current time is before the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-13"]
                    (toUtc "2021-06-14 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-14 15:30:00 PDT")

            it "returns a request time for Tuesday if the current time is after the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-13"]
                    (toUtc "2021-06-14 15:30:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-15 15:30:00 PDT")

            it "returns a request time for Tuesday if a timecard entry already exists for Monday" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-14"]
                    (toUtc "2021-06-14 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-15 15:30:00 PDT")

        describe "Tuesday" $ do
            it "returns a request time for Tuesday if the current time is before the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-14"]
                    (toUtc "2021-06-15 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-15 15:30:00 PDT")

            it "returns a request time for Wednesday if the current time is after the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-14"]
                    (toUtc "2021-06-15 15:30:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-16 15:30:00 PDT")

            it "returns a request time for Wednesday if a timecard entry already exists for Tuesday" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-15"]
                    (toUtc "2021-06-15 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-16 15:30:00 PDT")

        describe "Wednesday" $ do
            it "returns a request time for Wednesday if the current time is before the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-15"]
                    (toUtc "2021-06-16 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-16 15:30:00 PDT")

            it "returns a request time for Thursday if the current time is after the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-15"]
                    (toUtc "2021-06-16 15:30:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-17 15:30:00 PDT")

            it "returns a request time for Thursday if a timecard entry already exists for Wednesday" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-16"]
                    (toUtc "2021-06-16 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-17 15:30:00 PDT")

        describe "Thursday" $ do
            it "returns a request time for Thursday if the current time is before the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-16"]
                    (toUtc "2021-06-17 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-17 15:30:00 PDT")

            it "returns a request time for Friday if the current time is after the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-16"]
                    (toUtc "2021-06-17 15:30:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-18 15:30:00 PDT")

            it "returns a request time for Friday if a timecard entry already exists for Thursday" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-17"]
                    (toUtc "2021-06-17 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-18 15:30:00 PDT")

        describe "Friday" $ do
            it "returns a request time for Friday if the current time is before the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-17"]
                    (toUtc "2021-06-18 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-18 15:30:00 PDT")

            it "returns a request time for Monday if the current time is after the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-17"]
                    (toUtc "2021-06-18 15:30:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-21 15:30:00 PDT")

            it "returns a request time for Monday if a timecard entry already exists for Friday" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-18"]
                    (toUtc "2021-06-18 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-21 15:30:00 PDT")

        describe "Saturday" $ do
            it "returns a request time for Monday if the current time is before the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-18"]
                    (toUtc "2021-06-19 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-21 15:30:00 PDT")

            it "returns a request time for Monday if the current time is after the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-18"]
                    (toUtc "2021-06-19 15:30:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-21 15:30:00 PDT")

            it "returns a request time for Monday if a timecard entry already exists for Saturday" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-19"]
                    (toUtc "2021-06-19 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-21 15:30:00 PDT")

        describe "Sunday" $ do
            it "returns a request time for Monday if the current time is before the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-19"]
                    (toUtc "2021-06-20 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-21 15:30:00 PDT")

            it "returns a request time for Monday if the current time is after the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-19"]
                    (toUtc "2021-06-20 15:30:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-21 15:30:00 PDT")

            it "returns a request time for Monday if a timecard entry already exists for Sunday" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-20"]
                    (toUtc "2021-06-20 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-21 15:30:00 PDT")

        describe "past timecard entry dates" $ do
            it "disregards old timecard entries when the current time is before the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-01-01"]
                    (toUtc "2021-06-14 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-14 15:30:00 PDT")

            it "disregards old timecard entries when the current time is after the deadline" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-01-01"]
                    (toUtc "2021-06-14 15:30:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-15 15:30:00 PDT")

        describe "future timecard entry dates" $ do
            it "returns the first workday for which there is no timecard entry" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [ toDay "2021-06-14"
                    , toDay "2021-06-15"
                    , toDay "2021-06-17"
                    ]
                    (toUtc "2021-06-14 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-16 15:30:00 PDT")

            it "returns Monday if future timecard entries exist through Friday" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 15 30 00)
                    [ toDay "2021-06-14"
                    , toDay "2021-06-15"
                    , toDay "2021-06-16"
                    , toDay "2021-06-17"
                    , toDay "2021-06-18"
                    ]
                    (toUtc "2021-06-14 15:29:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-21 15:30:00 PDT")

        describe "different timezone" $ do
            it "returns the proper request time for the specified timezone" $ do
                nextRequestTime
                    False
                    est
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-13"]
                    (toUtc "2021-06-14 15:29:00 EST")
                    `shouldBe` Just (toUtc "2021-06-14 15:30:00 EST")

        describe "alternate request time of day" $ do
            it "returns a request time that corresponds to the given request time of day" $ do
                nextRequestTime
                    False
                    pdt
                    (TimeOfDay 5 40 20)
                    [toDay "2021-06-13"]
                    (toUtc "2021-06-14 05:39:00 PDT")
                    `shouldBe` Just (toUtc "2021-06-14 05:40:20 PDT")

        describe "pre-existing request" $ do
            it "returns nothing if a timecard entry request has already been scheduled" $ do
                nextRequestTime
                    True
                    pdt
                    (TimeOfDay 15 30 00)
                    [toDay "2021-06-13"]
                    (toUtc "2021-06-14 15:29:00 PDT")
                    `shouldBe` Nothing

    describe "scheduleNextRequest" $ do
        beforeAll (testConfig >>= mockContext RootApplication) do
            it "uses the worker's preferred daily reminder time" $ withContext do
                withTransactionRollback do
                    ron <-
                        newRecord @Person
                            |> set #firstName "Ronald"
                            |> set #lastName "McDonald"
                            |> set #goesBy "Ron"
                            |> createRecord

                    timPhoneNumber <-
                        newRecord @PhoneNumber
                            |> set #number "+18054035926"
                            |> createRecord

                    ronPhoneNumber <-
                        newRecord @PhoneNumber
                            |> set #number "+18054030600"
                            |> createRecord

                    newRecord @PhoneContact
                        |> set #phoneNumberId (get #id ronPhoneNumber)
                        |> set #personId (get #id ron)
                        |> createRecord

                    newRecord @WorkerPreference
                        |> set #personId (get #id ron)
                        |> set #sendDailyReminderAt (read "15:30:00")
                        |> createRecord

                    timecard <-
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-21")
                            |> set #personId (get #id ron)
                            |> createRecord

                    timecardEntry <-
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id timecard)
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> createRecord

                    scheduleNextRequest
                        pdt
                        (toUtc "2021-06-14 15:29:00 PDT")
                        timecardEntry
                        ron
                        (get #id timPhoneNumber)
                        (get #id ronPhoneNumber)

                    sendMessageAction <-
                        query @SendMessageAction
                            |> filterWhere (#toId, get #id ronPhoneNumber)
                            |> fetchOne

                    actionRunTime <-
                        query @ActionRunTime
                            |> filterWhere (#actionRunStateId, get #actionRunStateId sendMessageAction)
                            |> fetchOne

                    get #runsAt actionRunTime
                        `shouldBe` toUtc "2021-06-14 15:30:00 PDT"

                    get #body sendMessageAction
                        `shouldBe` "Hey Ron - I've got you at McDonald's today. Let me know what hours you worked and what you did when you have a chance. Thanks!"

            it "avoids scheduling multiple send message actions" $ withContext do
                withTransactionRollback do
                    ron <-
                        newRecord @Person
                            |> set #firstName "Ronald"
                            |> set #lastName "McDonald"
                            |> set #goesBy "Ron"
                            |> createRecord

                    timPhoneNumber <-
                        newRecord @PhoneNumber
                            |> set #number "+18054035926"
                            |> createRecord

                    ronPhoneNumber <-
                        newRecord @PhoneNumber
                            |> set #number "+18054030600"
                            |> createRecord

                    newRecord @PhoneContact
                        |> set #phoneNumberId (get #id ronPhoneNumber)
                        |> set #personId (get #id ron)
                        |> createRecord

                    newRecord @WorkerPreference
                        |> set #personId (get #id ron)
                        |> set #sendDailyReminderAt (read "15:30:00")
                        |> createRecord

                    timecard <-
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-21")
                            |> set #personId (get #id ron)
                            |> createRecord

                    timecardEntry <-
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id timecard)
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> createRecord

                    scheduleNextRequest
                        pdt
                        (toUtc "2021-06-14 15:29:00 PDT")
                        timecardEntry
                        ron
                        (get #id timPhoneNumber)
                        (get #id ronPhoneNumber)

                    scheduleNextRequest
                        pdt
                        (toUtc "2021-06-14 15:29:00 PDT")
                        timecardEntry
                        ron
                        (get #id timPhoneNumber)
                        (get #id ronPhoneNumber)

                    sendMessageActionCount <-
                        query @SendMessageAction
                            |> filterWhere (#toId, get #id ronPhoneNumber)
                            |> fetchCount

                    sendMessageActionCount `shouldBe` 1

    describe "requestBody" $ do
        it "returns the body of the request" $ do
            let person = newRecord @Person |> set #goesBy "Big Bird"
            let timecardEntry = newRecord @TimecardEntry |> set #jobName "Sesame St."
            requestBody person timecardEntry
                `shouldBe` "Hey Big Bird - I've got you at Sesame St. today. Let me know what hours you worked and what you did when you have a chance. Thanks!"

toUtc :: String -> UTCTime
toUtc time = zonedTimeToUTC (read time :: ZonedTime)

toDay :: String -> Day
toDay = read

pdt :: TimeZone
pdt = read "PDT"

est :: TimeZone
est = read "EST"