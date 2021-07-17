module Application.Timecard.EntryMessageSpec where

import qualified Application.Timecard.EntryMessage as Timecard.EntryMessage
import Data.Set (Set)
import Data.Set as Set
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "createAll" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "inserts a timecard entry message for each Twilio message" do
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
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
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

                timecardEntryMessages <-
                    Timecard.EntryMessage.createAll
                        (get #id timecardEntry)
                        [get #id twilioMessage1, get #id twilioMessage2]

                (not . any isNew) timecardEntryMessages `shouldBe` True

    describe "replaceAll" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "replaces existing timecard entry messages with new ones" do
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
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
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

                Timecard.EntryMessage.createAll
                    (get #id timecardEntry)
                    [get #id twilioMessage1, get #id twilioMessage2]

                timecardEntryMessages <-
                    Timecard.EntryMessage.replaceAll
                        (get #id timecardEntry)
                        [get #id twilioMessage1]

                newTimecardEntryMessages <-
                    query @TimecardEntryMessage
                        |> filterWhere (#timecardEntryId, get #id timecardEntry)
                        |> fetch

                newTimecardEntryMessages `shouldBe` timecardEntryMessages

                Set.fromList (get #id <$> newTimecardEntryMessages)
                    `shouldBe` Set.fromList (get #id <$> timecardEntryMessages)

    describe "fetchByTimecardEntry" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "returns timecard entry messages associated with the given timecard entry" do
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
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
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

                timecardEntryMessages <-
                    Timecard.EntryMessage.createAll
                        (get #id timecardEntry)
                        [get #id twilioMessage1, get #id twilioMessage2]

                fetchedMessages <-
                    Timecard.EntryMessage.fetchByTimecardEntry $
                        get #id timecardEntry

                Set.fromList (get #id <$> fetchedMessages)
                    `shouldBe` Set.fromList (get #id <$> timecardEntryMessages)
