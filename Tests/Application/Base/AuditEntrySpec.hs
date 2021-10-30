module Tests.Application.Base.AuditEntrySpec where

import Application.Base.AuditEntry
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "createMessageSentEntry" do
            itIO "returns a message sent entry" do
                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+16666666666"
                        |> createRecord

                twilioMessage <-
                    newRecord @TwilioMessage
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hi there!"
                        |> set #messageSid "1234"
                        |> createRecord

                auditEntry <-
                    createMessageSentEntry
                        Nothing
                        twilioMessage
                        "+15555555555"

                get #phoneNumberId auditEntry `shouldBe` get #id toPhoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` MessageSent
                get #actionContext auditEntry
                    `shouldBe` ( "MessageSentContext {twilioMessageId = "
                                    <> show (get #id twilioMessage)
                                    <> ", twilioMessageSid = \"1234\", fromPhoneNumber = \"+15555555555\", messageBody = \"Hi there!\"}"
                               )

        describe "createMessageReceivedEntry" do
            itIO "returns a message received entry" do
                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+16666666666"
                        |> createRecord

                twilioMessage <-
                    newRecord @TwilioMessage
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hi there!"
                        |> set #messageSid "1234"
                        |> createRecord

                auditEntry <-
                    createMessageReceivedEntry
                        twilioMessage
                        "+16666666666"

                get #phoneNumberId auditEntry `shouldBe` get #id fromPhoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` MessageReceived
                get #actionContext auditEntry
                    `shouldBe` ( "MessageReceivedContext {twilioMessageId = \""
                                    <> show (get #id twilioMessage)
                                    <> "\", twilioMessageSid = \"1234\", toPhoneNumber = \"+16666666666\", messageBody = \"Hi there!\"}"
                               )

        describe "createMessageProcessedEntry" do
            itIO "returns a message processed entry" do
                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+16666666666"
                        |> createRecord

                twilioMessage <-
                    newRecord @TwilioMessage
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hi there!"
                        |> set #messageSid "1234"
                        |> createRecord

                auditEntry <-
                    createMessageProcessedEntry
                        twilioMessage
                        "SomeSituation"
                        "SomePlan"

                get #phoneNumberId auditEntry `shouldBe` get #id fromPhoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` MessageProcessed
                get #actionContext auditEntry
                    `shouldBe` ( "MessageProcessedContext {twilioMessageId = \""
                                    <> show (get #id twilioMessage)
                                    <> "\", messageBody = \"Hi there!\", situation = \"SomeSituation\", plan = \"SomePlan\"}"
                               )

        describe "createMessageReceivedEntry" do
            itIO "returns a message received entry" do
                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+16666666666"
                        |> createRecord

                twilioMessage <-
                    newRecord @TwilioMessage
                        |> set #fromId (get #id fromPhoneNumber)
                        |> set #toId (get #id toPhoneNumber)
                        |> set #body "Hi there!"
                        |> set #messageSid "1234"
                        |> createRecord

                auditEntry <-
                    createMessageReceivedEntry
                        twilioMessage
                        "+16666666666"

                get #phoneNumberId auditEntry `shouldBe` get #id fromPhoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` MessageReceived
                get #actionContext auditEntry
                    `shouldBe` ( "MessageReceivedContext {twilioMessageId = \""
                                    <> show (get #id twilioMessage)
                                    <> "\", twilioMessageSid = \"1234\", toPhoneNumber = \"+16666666666\", messageBody = \"Hi there!\"}"
                               )

        describe "createTimecardCreatedEntry" do
            itIO "returns a timecard created entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                let timecardEntry =
                        newRecord @TimecardEntry
                            |> set #date (toDay "2021-10-30")
                            |> set #jobName "Costco"
                            |> set #clockedInAt (Just $ toTimeOfDay "07:00:00")
                            |> set #clockedOutAt (Just $ toTimeOfDay "07:00:01")
                            |> set #lunchDuration (Just 30)
                            |> set #hoursWorked 8.0
                            |> set #workDone "Ate chips."
                            |> set #invoiceTranslation "Installed doors."

                auditEntry <-
                    createTimecardEntryCreatedEntry
                        Nothing
                        (get #id phoneNumber)
                        timecardEntry

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` TimecardEntryCreated
                get #actionContext auditEntry
                    `shouldBe` "TimecardEntryContext {timecardEntryId = 00000000-0000-0000-0000-000000000000, date = 2021-10-30, jobName = \"Costco\", clockedInAt = Just 07:00:00, clockedOutAt = Just 07:00:01, lunchDuration = Just 30, hoursWorked = 8.0, workDone = \"Ate chips.\", invoiceTranslation = \"Installed doors.\"}"

        describe "createTimecardEditedEntry" do
            itIO "returns a timecard edited entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                let timecardEntry =
                        newRecord @TimecardEntry
                            |> set #date (toDay "2021-10-30")
                            |> set #jobName "Costco"
                            |> set #clockedInAt (Just $ toTimeOfDay "07:00:00")
                            |> set #clockedOutAt (Just $ toTimeOfDay "07:00:01")
                            |> set #lunchDuration (Just 30)
                            |> set #hoursWorked 8.0
                            |> set #workDone "Ate chips."
                            |> set #invoiceTranslation "Installed doors."

                auditEntry <-
                    createTimecardEntryEditedEntry
                        Nothing
                        (get #id phoneNumber)
                        timecardEntry

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` TimecardEntryEdited
                get #actionContext auditEntry
                    `shouldBe` "TimecardEntryContext {timecardEntryId = 00000000-0000-0000-0000-000000000000, date = 2021-10-30, jobName = \"Costco\", clockedInAt = Just 07:00:00, clockedOutAt = Just 07:00:01, lunchDuration = Just 30, hoursWorked = 8.0, workDone = \"Ate chips.\", invoiceTranslation = \"Installed doors.\"}"

        describe "createTimecardDeletedEntry" do
            itIO "returns a timecard deleted entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                let timecardEntry =
                        newRecord @TimecardEntry
                            |> set #date (toDay "2021-10-30")
                            |> set #jobName "Costco"
                            |> set #clockedInAt (Just $ toTimeOfDay "07:00:00")
                            |> set #clockedOutAt (Just $ toTimeOfDay "07:00:01")
                            |> set #lunchDuration (Just 30)
                            |> set #hoursWorked 8.0
                            |> set #workDone "Ate chips."
                            |> set #invoiceTranslation "Installed doors."

                auditEntry <-
                    createTimecardEntryDeletedEntry
                        Nothing
                        (get #id phoneNumber)
                        timecardEntry

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` TimecardEntryDeleted
                get #actionContext auditEntry
                    `shouldBe` "TimecardEntryContext {timecardEntryId = 00000000-0000-0000-0000-000000000000, date = 2021-10-30, jobName = \"Costco\", clockedInAt = Just 07:00:00, clockedOutAt = Just 07:00:01, lunchDuration = Just 30, hoursWorked = 8.0, workDone = \"Ate chips.\", invoiceTranslation = \"Installed doors.\"}"

        describe "createReviewLinkGeneratedEntry" do
            itIO "returns a review link generated entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                auditEntry <-
                    createReviewLinkGeneratedEntry
                        Nothing
                        (get #id phoneNumber)
                        "https://barf.com"

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` ReviewLinkGenerated
                get #actionContext auditEntry
                    `shouldBe` "https://barf.com"

        describe "createReviewSignedEntry" do
            itIO "returns a review signed entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                auditEntry <- createReviewSignedEntry (get #id phoneNumber) "Dustin Hoffman"

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` ReviewSigned
                get #actionContext auditEntry `shouldBe` "Dustin Hoffman"

        describe "createEntry" do
            itIO "saves and returns an audit entry" do
                user <-
                    newRecord @User
                        |> set #email "test@email.com"
                        |> set #passwordHash "password hash"
                        |> createRecord

                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+16666666666"
                        |> createRecord

                auditEntry <-
                    createEntry
                        (Just $ get #id user)
                        (get #id phoneNumber)
                        MessageSent
                        "Some context."

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Just (get #id user)
                get #action auditEntry `shouldBe` MessageSent
                get #actionContext auditEntry `shouldBe` "Some context."
