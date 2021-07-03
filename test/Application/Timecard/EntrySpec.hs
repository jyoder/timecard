module Application.Timecard.EntrySpec where

import qualified Application.Timecard.Entry as Timecard.Entry
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support
import Text.Read (read)

spec :: Spec
spec = do
    describe "create" $ do
        beforeAll (testConfig >>= mockContext RootApplication) do
            it "sets the timecard id to the corresponding existing timecard" $ withContext do
                withTransactionRollback do
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
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> set #hoursWorked 8.0
                            |> set #workDone "work"
                            |> set #invoiceTranslation "invoice"
                            |> Timecard.Entry.create
                                (get #id ron)
                                []

                    get #timecardId <$> timecardEntry
                        `shouldBe` Right (get #id timecard)

            it "creates and assigns an existing timecard if one does not exist" $ withContext do
                withTransactionRollback do
                    ron <-
                        newRecord @Person
                            |> set #firstName "Ronald"
                            |> set #lastName "McDonald"
                            |> set #goesBy "Ron"
                            |> createRecord

                    timecardEntry <-
                        newRecord @TimecardEntry
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName "McDonald's"
                            |> set #hoursWorked 8.0
                            |> set #workDone "work"
                            |> set #invoiceTranslation "invoice"
                            |> Timecard.Entry.create
                                (get #id ron)
                                []

                    case timecardEntry of
                        Left _ -> expectationFailure "should not fail to create timecard entry"
                        Right timecardEntry -> do
                            timecard <- fetchOneOrNothing $ get #timecardId timecardEntry
                            isJust timecard `shouldBe` True

            it "returns left if the timecard entry is not valid" $ withContext do
                withTransactionRollback do
                    ron <-
                        newRecord @Person
                            |> set #firstName "Ronald"
                            |> set #lastName "McDonald"
                            |> set #goesBy "Ron"
                            |> createRecord

                    timecardEntry <-
                        newRecord @TimecardEntry
                            |> set #date (toDay "2021-06-23")
                            |> set #jobName ""
                            |> set #hoursWorked 8.0
                            |> set #workDone "work"
                            |> set #invoiceTranslation "invoice"
                            |> Timecard.Entry.create
                                (get #id ron)
                                []

                    case timecardEntry of
                        Left _ -> pure ()
                        Right _ -> expectationFailure "should fail to create timecard entry"

            it "inserts the timecard entry along with messages and returns right if it is valid" $ withContext do
                withTransactionRollback do
                    ron <-
                        newRecord @Person
                            |> set #firstName "Ronald"
                            |> set #lastName "McDonald"
                            |> set #goesBy "Ron"
                            |> createRecord

                    timecard1 <-
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-21")
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
                                (get #id ron)
                                [get #id twilioMessage]

                    twilioMessages <- query @TimecardEntryMessage |> fetch
                    get #twilioMessageId <$> twilioMessages
                        `shouldBe` [get #id twilioMessage]

    describe "update" $ do
        beforeAll (testConfig >>= mockContext RootApplication) do
            it "sets the timecard id to the corresponding existing timecard" $ withContext do
                withTransactionRollback do
                    ron <-
                        newRecord @Person
                            |> set #firstName "Ronald"
                            |> set #lastName "McDonald"
                            |> set #goesBy "Ron"
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
                            |> Timecard.Entry.update []

                    case timecardEntry of
                        Left _ -> expectationFailure "should not fail to update timecard entry"
                        Right timecardEntry -> do
                            timecard2 <- fetch $ get #timecardId timecardEntry
                            get #weekOf timecard2 `shouldBe` toDay "2021-06-28"
                            get #timecardId timecardEntry `shouldBe` get #id timecard2

            it "creates and assigns an existing timecard if one does not exist" $ withContext do
                withTransactionRollback do
                    ron <-
                        newRecord @Person
                            |> set #firstName "Ronald"
                            |> set #lastName "McDonald"
                            |> set #goesBy "Ron"
                            |> createRecord

                    timecard1 <-
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-21")
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
                            |> Timecard.Entry.update []

                    case timecardEntry of
                        Left _ -> expectationFailure "should not fail to update timecard entry"
                        Right timecardEntry -> do
                            timecard2 <- fetch $ get #timecardId timecardEntry
                            get #weekOf timecard2 `shouldBe` toDay "2021-06-28"
                            get #timecardId timecardEntry `shouldNotBe` get #id timecard1

            it "returns left if the timecard entry is not valid" $ withContext do
                withTransactionRollback do
                    ron <-
                        newRecord @Person
                            |> set #firstName "Ronald"
                            |> set #lastName "McDonald"
                            |> set #goesBy "Ron"
                            |> createRecord

                    timecard1 <-
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-21")
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
                            |> set #jobName ""
                            |> set #hoursWorked 8.0
                            |> set #workDone "work"
                            |> set #invoiceTranslation "invoice"
                            |> Timecard.Entry.update []

                    case timecardEntry of
                        Left _ -> pure ()
                        Right timecardEntry -> expectationFailure "should fail to update timecard entry"

            it "updates the timecard entry along with messages if it is valid" $ withContext do
                withTransactionRollback do
                    ron <-
                        newRecord @Person
                            |> set #firstName "Ronald"
                            |> set #lastName "McDonald"
                            |> set #goesBy "Ron"
                            |> createRecord

                    timecard1 <-
                        newRecord @Timecard
                            |> set #weekOf (toDay "2021-06-21")
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
                            |> Timecard.Entry.update [get #id twilioMessage2]

                    twilioMessages <- query @TimecardEntryMessage |> fetch
                    get #twilioMessageId <$> twilioMessages
                        `shouldBe` [get #id twilioMessage2]

    describe "validate" $ do
        beforeAll (testConfig >>= mockContext RootApplication) do
            it "validates all fields" $ withContext do
                withTransactionRollback do
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
                                |> set #hoursWorked (-1.0)
                                |> set #workDone ""
                                |> set #invoiceTranslation ""

                    timecardEntry <- Timecard.Entry.validate timecardEntry
                    timecardEntry |> get #meta |> get #annotations
                        `shouldBe` [ ("date", "date must fall within the timecard week 2021-06-21")
                                   , ("invoiceTranslation", "This field cannot be empty")
                                   , ("workDone", "This field cannot be empty")
                                   , ("hoursWorked", "This field must be greater than or equal to 0.0")
                                   , ("jobName", "This field cannot be empty")
                                   ]

toDay :: String -> Day
toDay = read
