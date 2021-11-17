module Tests.Application.Timecard.EntrySpec where

import qualified Application.Timecard.Entry as Timecard.Entry
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "create" do
            itIO "sets the timecard id to the corresponding existing timecard" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #date (toDay "2021-06-23")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> Timecard.Entry.create
                            Nothing
                            (get #id ron)
                            (get #id ronPhoneNumber)
                            []

                get #timecardId <$> timecardEntry
                    `shouldBe` Right (get #id timecard)

            itIO "inserts an audit entry when a timecard is created" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                auditEntryCount <-
                    query @AuditEntry
                        |> filterWhere (#phoneNumberId, get #id ronPhoneNumber)
                        |> fetchCount

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #date (toDay "2021-06-23")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> Timecard.Entry.create
                            Nothing
                            (get #id ron)
                            (get #id ronPhoneNumber)
                            []

                auditEntryCount' <-
                    query @AuditEntry
                        |> filterWhere (#phoneNumberId, get #id ronPhoneNumber)
                        |> filterWhere (#action, TimecardEntryCreated)
                        |> fetchCount
                auditEntryCount' `shouldBe` auditEntryCount + 1

            itIO "creates and assigns an existing timecard if one does not exist" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #date (toDay "2021-06-23")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> Timecard.Entry.create
                            Nothing
                            (get #id ron)
                            (get #id ronPhoneNumber)
                            []

                case timecardEntry of
                    Left _ -> expectationFailure "should not fail to create timecard entry"
                    Right timecardEntry -> do
                        timecard <- fetchOneOrNothing $ get #timecardId timecardEntry
                        isJust timecard `shouldBe` True

            itIO "returns left if the timecard entry is not valid" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #date (toDay "2021-06-23")
                        |> set #jobName ""
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> Timecard.Entry.create
                            Nothing
                            (get #id ron)
                            (get #id ronPhoneNumber)
                            []

                case timecardEntry of
                    Left _ -> pure ()
                    Right _ -> expectationFailure "should fail to create timecard entry"

            itIO "inserts the timecard entry along with messages and returns right if it is valid" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-21")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+18054035926"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+18054030600"
                        |> createRecord

                twilioMessage <-
                    newRecord @TwilioMessage
                        |> set #apiVersion "1.0"
                        |> set #messageSid "sid1"
                        |> set #messagingServiceSid Nothing
                        |> set #fromId (get #id timPhoneNumber)
                        |> set #toId (get #id ronPhoneNumber)
                        |> set #status "sent"
                        |> set #body "Can I get some burgers?"
                        |> set #numMedia 0
                        |> createRecord

                timecardEntry <-
                    timecardEntry
                        |> set #date (toDay "2021-06-29")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> Timecard.Entry.create
                            Nothing
                            (get #id ron)
                            (get #id ronPhoneNumber)
                            [get #id twilioMessage]

                twilioMessages <- query @TimecardEntryMessage |> fetch
                get #twilioMessageId <$> twilioMessages
                    `shouldBe` [get #id twilioMessage]

        describe "update" do
            itIO "sets the timecard id to the corresponding existing timecard" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                timecard1 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecard2 <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-28")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard1)
                        |> set #date (toDay "2021-06-21")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry <-
                    timecardEntry
                        |> set #date (toDay "2021-06-29")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> Timecard.Entry.update
                            Nothing
                            (get #id ronPhoneNumber)
                            []

                case timecardEntry of
                    Left _ -> expectationFailure "should not fail to update timecard entry"
                    Right timecardEntry -> do
                        timecard2 <- fetch $ get #timecardId timecardEntry
                        get #weekOf timecard2 `shouldBe` toDay "2021-06-28"
                        get #timecardId timecardEntry `shouldBe` get #id timecard2

            itIO "inserts an audit entry when a timecard is created" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-21")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                auditEntryCount <-
                    query @AuditEntry
                        |> filterWhere (#phoneNumberId, get #id ronPhoneNumber)
                        |> filterWhere (#action, TimecardEntryEdited)
                        |> fetchCount

                timecardEntry <-
                    timecardEntry
                        |> set #date (toDay "2021-06-29")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> Timecard.Entry.update
                            Nothing
                            (get #id ronPhoneNumber)
                            []

                auditEntryCount' <-
                    query @AuditEntry
                        |> filterWhere (#phoneNumberId, get #id ronPhoneNumber)
                        |> filterWhere (#action, TimecardEntryEdited)
                        |> fetchCount
                auditEntryCount' `shouldBe` auditEntryCount + 1

            itIO "creates and assigns an existing timecard if one does not exist" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-21")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry <-
                    timecardEntry
                        |> set #date (toDay "2021-06-29")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> Timecard.Entry.update
                            Nothing
                            (get #id ronPhoneNumber)
                            []

                case timecardEntry of
                    Left _ -> expectationFailure "should not fail to update timecard entry"
                    Right timecardEntry -> do
                        timecard2 <- fetch $ get #timecardId timecardEntry
                        get #weekOf timecard2 `shouldBe` toDay "2021-06-28"
                        get #timecardId timecardEntry `shouldNotBe` get #id timecard

            itIO "returns left if the timecard entry is not valid" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-21")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timecardEntry <-
                    timecardEntry
                        |> set #date (toDay "2021-06-29")
                        |> set #jobName ""
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> Timecard.Entry.update
                            Nothing
                            (get #id ronPhoneNumber)
                            []

                case timecardEntry of
                    Left _ -> pure ()
                    Right timecardEntry -> expectationFailure "should fail to update timecard entry"

            itIO "updates the timecard entry along with messages if it is valid" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-21")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+18054035926"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+18054030600"
                        |> createRecord

                twilioMessage1 <-
                    newRecord @TwilioMessage
                        |> set #apiVersion "1.0"
                        |> set #messageSid "sid1"
                        |> set #messagingServiceSid Nothing
                        |> set #fromId (get #id timPhoneNumber)
                        |> set #toId (get #id ronPhoneNumber)
                        |> set #status "sent"
                        |> set #body "Can I get some burgers?"
                        |> set #numMedia 0
                        |> createRecord

                twilioMessage2 <-
                    newRecord @TwilioMessage
                        |> set #apiVersion "1.0"
                        |> set #messageSid "sid2"
                        |> set #messagingServiceSid Nothing
                        |> set #fromId (get #id ronPhoneNumber)
                        |> set #toId (get #id timPhoneNumber)
                        |> set #status "sent"
                        |> set #body "You got it!"
                        |> set #numMedia 0
                        |> createRecord

                newRecord @TimecardEntryMessage
                    |> set #timecardEntryId (get #id timecardEntry)
                    |> set #twilioMessageId (get #id twilioMessage1)
                    |> createRecord

                timecardEntry <-
                    timecardEntry
                        |> set #date (toDay "2021-06-29")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> Timecard.Entry.update
                            Nothing
                            (get #id ronPhoneNumber)
                            [get #id twilioMessage2]

                timecardEntryMessages <- query @TimecardEntryMessage |> fetch
                get #twilioMessageId <$> timecardEntryMessages
                    `shouldBe` [get #id twilioMessage2]

        describe "delete" do
            itIO "deletes the given timecard entry and all message associations" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-21")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+18054035926"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+18054030600"
                        |> createRecord

                twilioMessage <-
                    newRecord @TwilioMessage
                        |> set #apiVersion "1.0"
                        |> set #messageSid "sid1"
                        |> set #messagingServiceSid Nothing
                        |> set #fromId (get #id timPhoneNumber)
                        |> set #toId (get #id ronPhoneNumber)
                        |> set #status "sent"
                        |> set #body "Can I get some burgers?"
                        |> set #numMedia 0
                        |> createRecord

                newRecord @TimecardEntryMessage
                    |> set #timecardEntryId (get #id timecardEntry)
                    |> set #twilioMessageId (get #id twilioMessage)
                    |> createRecord

                Timecard.Entry.delete
                    Nothing
                    (get #id ronPhoneNumber)
                    (get #id timecardEntry)

                timecardEntry' <- fetchOneOrNothing $ get #id timecardEntry
                timecardEntry' `shouldBe` Nothing

                timecardEntryMessages <-
                    query @TimecardEntryMessage
                        |> filterWhere (#timecardEntryId, get #id timecardEntry)
                        |> fetch
                timecardEntryMessages `shouldBe` []

            itIO "inserts an audit entry when a timecard entry is deleted" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                timecardEntry <-
                    newRecord @TimecardEntry
                        |> set #timecardId (get #id timecard)
                        |> set #date (toDay "2021-06-21")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> createRecord

                timPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+18054035926"
                        |> createRecord

                ronPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+18054030600"
                        |> createRecord

                twilioMessage <-
                    newRecord @TwilioMessage
                        |> set #apiVersion "1.0"
                        |> set #messageSid "sid1"
                        |> set #messagingServiceSid Nothing
                        |> set #fromId (get #id timPhoneNumber)
                        |> set #toId (get #id ronPhoneNumber)
                        |> set #status "sent"
                        |> set #body "Can I get some burgers?"
                        |> set #numMedia 0
                        |> createRecord

                newRecord @TimecardEntryMessage
                    |> set #timecardEntryId (get #id timecardEntry)
                    |> set #twilioMessageId (get #id twilioMessage)
                    |> createRecord

                auditEntryCount <-
                    query @AuditEntry
                        |> filterWhere (#phoneNumberId, get #id ronPhoneNumber)
                        |> filterWhere (#action, TimecardEntryDeleted)
                        |> fetchCount

                Timecard.Entry.delete
                    Nothing
                    (get #id ronPhoneNumber)
                    (get #id timecardEntry)

                auditEntryCount' <-
                    query @AuditEntry
                        |> filterWhere (#phoneNumberId, get #id ronPhoneNumber)
                        |> filterWhere (#action, TimecardEntryDeleted)
                        |> fetchCount
                auditEntryCount' `shouldBe` auditEntryCount + 1

        describe "validate" do
            itIO "does basic validation on all fields" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                let timecardEntry =
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id timecard)
                            |> set #date (toDay "2021-05-10")
                            |> set #jobName ""
                            |> set #lunchDuration (Just (-1))
                            |> set #hoursWorked (-1.0)
                            |> set #workDone ""
                            |> set #invoiceTranslation ""

                timecardEntry <- Timecard.Entry.validate timecardEntry
                timecardEntry |> get #meta |> get #annotations
                    `shouldBe` [ ("date", TextViolation "date must fall within the timecard week 2021-06-21")
                               , ("invoiceTranslation", TextViolation "This field cannot be empty")
                               , ("workDone", TextViolation "This field cannot be empty")
                               , ("hoursWorked", TextViolation "This field must be greater than or equal to 0.0")
                               , ("lunchDuration", TextViolation "This field must be greater than or equal to 0")
                               , ("jobName", TextViolation "This field cannot be empty")
                               ]

            itIO "validates that clock details match hours worked" do
                ron <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id ron)
                        |> createRecord

                let timecardEntry =
                        newRecord @TimecardEntry
                            |> set #timecardId (get #id timecard)
                            |> set #date (toDay "2021-06-21")
                            |> set #jobName "Some Job"
                            |> set #clockedInAt (Just $ toTimeOfDay "07:00:00")
                            |> set #clockedOutAt (Just $ toTimeOfDay "06:00:00")
                            |> set #lunchDuration (Just 30)
                            |> set #hoursWorked 8.0
                            |> set #workDone "Hello"
                            |> set #invoiceTranslation "Hello"

                timecardEntry <- Timecard.Entry.validate timecardEntry
                timecardEntry |> get #meta |> get #annotations
                    `shouldBe` [ ("hoursWorked", TextViolation "Must be within 15 minutes of clock details")
                               , ("clockedInAt", TextViolation "Must be earlier than clock out time")
                               ]

    describe "isBefore" do
        it "returns success if the start time is before the end time" do
            Timecard.Entry.isBefore
                (Just $ toTimeOfDay "15:00:00")
                (Just $ toTimeOfDay "07:00:00")
                `shouldBe` Success

        it "returns failure if the start time is equal to the end time" do
            Timecard.Entry.isBefore
                (Just $ toTimeOfDay "15:00:00")
                (Just $ toTimeOfDay "15:00:00")
                `shouldBe` Failure "must be before 15:00:00"

        it "returns failure if the start time is after the end time" do
            Timecard.Entry.isBefore
                (Just $ toTimeOfDay "15:00:00")
                (Just $ toTimeOfDay "16:00:00")
                `shouldBe` Failure "must be before 15:00:00"

        it "returns failure if clock details do not match hours worked" do
            Timecard.Entry.matchesClockDetails
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "16:00:00")
                Nothing
                8.0
                `shouldBe` Failure "Must be within 15 minutes of clock details"

    describe "matchesClockDetails" do
        it "returns success if clock details match hours worked" do
            Timecard.Entry.matchesClockDetails
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "15:00:00")
                Nothing
                8.0
                `shouldBe` Success

        it "returns failure if clock details do not match hours worked" do
            Timecard.Entry.matchesClockDetails
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "16:00:00")
                Nothing
                8.0
                `shouldBe` Failure "Must be within 15 minutes of clock details"

    describe "clockDetailsMatchHoursWorked" do
        it "returns true if the clock in and clock out times match hours worked perfectly" do
            Timecard.Entry.clockDetailsMatchHoursWorked
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "15:00:00")
                Nothing
                8.0
                `shouldBe` True

        it "returns true if the clock in, clock out, and lunch times match hours worked perfectly" do
            Timecard.Entry.clockDetailsMatchHoursWorked
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "15:00:00")
                (Just 30)
                7.5
                `shouldBe` True

        it "returns true if the clock in, clock out, and lunch times match hours worked within a 15 minute tolerance" do
            Timecard.Entry.clockDetailsMatchHoursWorked
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "15:00:00")
                (Just 30)
                7.75
                `shouldBe` True
            Timecard.Entry.clockDetailsMatchHoursWorked
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "15:00:00")
                (Just 30)
                7.25
                `shouldBe` True

        it "returns true if the clock details are missing" do
            Timecard.Entry.clockDetailsMatchHoursWorked
                Nothing
                Nothing
                Nothing
                7.75
                `shouldBe` True

        it "returns false if the clock in, clock out, and lunch times do not match hours worked within a 15 minute tolerance" do
            Timecard.Entry.clockDetailsMatchHoursWorked
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "15:00:00")
                (Just 30)
                7.76
                `shouldBe` False
            Timecard.Entry.clockDetailsMatchHoursWorked
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "15:00:00")
                (Just 30)
                7.24
                `shouldBe` False

    describe "clockDetailsToTimeWorked" do
        it "assumes zero minutes for lunch when no lunch is provided" do
            Timecard.Entry.clockDetailsToTimeWorked
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "15:00:00")
                Nothing
                `shouldBe` Just (fromHours 8.0)

        it "subtracts given lunch value" do
            Timecard.Entry.clockDetailsToTimeWorked
                (Just $ toTimeOfDay "07:00:00")
                (Just $ toTimeOfDay "15:00:00")
                (Just 30)
                `shouldBe` Just (fromHours 7.5)

        it "returns nothing when clock in is missing" do
            Timecard.Entry.clockDetailsToTimeWorked
                Nothing
                (Just $ toTimeOfDay "15:00:00")
                (Just 30)
                `shouldBe` Nothing

        it "returns nothing when clock out is missing" do
            Timecard.Entry.clockDetailsToTimeWorked
                (Just $ toTimeOfDay "07:00:00")
                Nothing
                (Just 30)
                `shouldBe` Nothing

        it "returns nothing when clock in and clock out are missing" do
            Timecard.Entry.clockDetailsToTimeWorked
                Nothing
                Nothing
                (Just 30)
                `shouldBe` Nothing

        it "returns nothing when all clock details are missing" do
            Timecard.Entry.clockDetailsToTimeWorked
                Nothing
                Nothing
                Nothing
                `shouldBe` Nothing

        it "returns a negative elapsed time when the clock out is before the clock in" do
            Timecard.Entry.clockDetailsToTimeWorked
                (Just $ toTimeOfDay "15:00:00")
                (Just $ toTimeOfDay "07:00:00")
                (Just 30)
                `shouldBe` Just (fromHours (-8.5))

fromHours :: Double -> Integer
fromHours hours = round (hours * 60 * 60) * 1000000000000
