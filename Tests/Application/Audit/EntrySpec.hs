module Tests.Application.Audit.EntrySpec where

import Application.Audit.Entry
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "createMessageSent" do
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
                    createMessageSent
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

        describe "createMessageReceived" do
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
                    createMessageReceived
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

        describe "createMessageProcessed" do
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
                    createMessageProcessed
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

        describe "createMessageReceived" do
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
                    createMessageReceived
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

        describe "createTimecardCreated" do
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
                    createTimecardEntryCreated
                        Nothing
                        (get #id phoneNumber)
                        timecardEntry

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` TimecardEntryCreated
                get #actionContext auditEntry
                    `shouldBe` "TimecardEntryContext {timecardEntryId = 00000000-0000-0000-0000-000000000000, date = 2021-10-30, jobName = \"Costco\", clockedInAt = Just 07:00:00, clockedOutAt = Just 07:00:01, lunchDuration = Just 30, hoursWorked = 8.0, workDone = \"Ate chips.\", invoiceTranslation = \"Installed doors.\"}"

        describe "createTimecardEdited" do
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
                    createTimecardEntryEdited
                        Nothing
                        (get #id phoneNumber)
                        timecardEntry

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` TimecardEntryEdited
                get #actionContext auditEntry
                    `shouldBe` "TimecardEntryContext {timecardEntryId = 00000000-0000-0000-0000-000000000000, date = 2021-10-30, jobName = \"Costco\", clockedInAt = Just 07:00:00, clockedOutAt = Just 07:00:01, lunchDuration = Just 30, hoursWorked = 8.0, workDone = \"Ate chips.\", invoiceTranslation = \"Installed doors.\"}"

        describe "createTimecardDeleted" do
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
                    createTimecardEntryDeleted
                        Nothing
                        (get #id phoneNumber)
                        timecardEntry

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` TimecardEntryDeleted
                get #actionContext auditEntry
                    `shouldBe` "TimecardEntryContext {timecardEntryId = 00000000-0000-0000-0000-000000000000, date = 2021-10-30, jobName = \"Costco\", clockedInAt = Just 07:00:00, clockedOutAt = Just 07:00:01, lunchDuration = Just 30, hoursWorked = 8.0, workDone = \"Ate chips.\", invoiceTranslation = \"Installed doors.\"}"

        describe "createReviewLinkGenerated" do
            itIO "returns a review link generated entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                auditEntry <-
                    createReviewLinkGenerated
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

                auditEntry <- createReviewSigned (get #id phoneNumber) "Dustin Hoffman"

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` ReviewSigned
                get #actionContext auditEntry `shouldBe` "Dustin Hoffman"

        describe "createDailyReminderScheduled" do
            itIO "returns a daily reminder scheduled entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                let sendMessageAction =
                        newRecord @SendMessageAction
                            |> set #id "10000000-0000-0000-0000-000000000000"
                            |> set #toId (get #id phoneNumber)
                            |> set #body "Yo!"

                auditEntry <-
                    createDailyReminderScheduled
                        sendMessageAction
                        (toUtc "2021-10-30 07:00:00 PDT")

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` DailyReminderScheduled
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext {sendMessageActionId = 10000000-0000-0000-0000-000000000000, sendAt = 2021-10-30 14:00:00 UTC, body = \"Yo!\"}"

        describe "createReviewRequestScheduled" do
            itIO "returns a review request scheduled entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                let sendMessageAction =
                        newRecord @SendMessageAction
                            |> set #id "10000000-0000-0000-0000-000000000000"
                            |> set #toId (get #id phoneNumber)
                            |> set #body "Yo!"

                auditEntry <-
                    createReviewRequestScheduled
                        sendMessageAction
                        (toUtc "2021-10-30 07:00:00 PDT")

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` ReviewRequestScheduled
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext {sendMessageActionId = 10000000-0000-0000-0000-000000000000, sendAt = 2021-10-30 14:00:00 UTC, body = \"Yo!\"}"

        describe "createScheduledMessageEdited" do
            itIO "returns a scheduled message edited entry" do
                user <-
                    newRecord @User
                        |> set #email "test@company.com"
                        |> set #passwordHash "some password hash"
                        |> createRecord

                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                let sendMessageAction =
                        newRecord @SendMessageAction
                            |> set #id "10000000-0000-0000-0000-000000000000"
                            |> set #toId (get #id phoneNumber)
                            |> set #body "Yo!"

                auditEntry <-
                    createScheduledMessageEdited
                        (get #id user)
                        sendMessageAction
                        (toUtc "2021-10-30 07:00:00 PDT")

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Just (get #id user)
                get #action auditEntry `shouldBe` ScheduledMessageEdited
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext {sendMessageActionId = 10000000-0000-0000-0000-000000000000, sendAt = 2021-10-30 14:00:00 UTC, body = \"Yo!\"}"

        describe "createScheduledMessageSuspended" do
            itIO "returns a scheduled message suspended entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                let sendMessageAction =
                        newRecord @SendMessageAction
                            |> set #id "10000000-0000-0000-0000-000000000000"
                            |> set #toId (get #id phoneNumber)
                            |> set #body "Yo!"

                auditEntry <-
                    createScheduledMessageSuspended
                        sendMessageAction
                        (toUtc "2021-10-30 07:00:00 PDT")

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` ScheduledMessageSuspended
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext {sendMessageActionId = 10000000-0000-0000-0000-000000000000, sendAt = 2021-10-30 14:00:00 UTC, body = \"Yo!\"}"

        describe "createScheduledMessageResumed" do
            itIO "returns a scheduled message resumed entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                let sendMessageAction =
                        newRecord @SendMessageAction
                            |> set #id "10000000-0000-0000-0000-000000000000"
                            |> set #toId (get #id phoneNumber)
                            |> set #body "Yo!"

                auditEntry <-
                    createScheduledMessageResumed
                        Nothing
                        sendMessageAction
                        (toUtc "2021-10-30 07:00:00 PDT")

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` ScheduledMessageResumed
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext {sendMessageActionId = 10000000-0000-0000-0000-000000000000, sendAt = 2021-10-30 14:00:00 UTC, body = \"Yo!\"}"

        describe "createScheduledMessageCanceled" do
            itIO "returns a scheduled message canceled entry" do
                phoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                let sendMessageAction =
                        newRecord @SendMessageAction
                            |> set #id "10000000-0000-0000-0000-000000000000"
                            |> set #toId (get #id phoneNumber)
                            |> set #body "Yo!"

                auditEntry <-
                    createScheduledMessageCanceled
                        Nothing
                        sendMessageAction
                        (toUtc "2021-10-30 07:00:00 PDT")

                get #phoneNumberId auditEntry `shouldBe` get #id phoneNumber
                get #userId auditEntry `shouldBe` Nothing
                get #action auditEntry `shouldBe` ScheduledMessageCanceled
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext {sendMessageActionId = 10000000-0000-0000-0000-000000000000, sendAt = 2021-10-30 14:00:00 UTC, body = \"Yo!\"}"

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
