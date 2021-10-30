module Tests.Application.Timecard.EntrySpec where

import qualified Application.Timecard.Entry as Timecard.Entry
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
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
            itIO "validates all fields" do
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
