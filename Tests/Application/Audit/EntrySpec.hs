module Tests.Application.Audit.EntrySpec where

import Application.Audit.Entry
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support
import Text.Pretty.Simple (pShowNoColor)

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
                    `shouldBe` ( "MessageSentContext\n    { twilioMessageId = "
                                    <> cs (pShowNoColor $ get #id twilioMessage)
                                    <> "\n    , twilioMessageSid = \"1234\"\n    , fromPhoneNumber = \"+15555555555\"\n    , messageBody = \"Hi there!\"\n    }"
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
                    `shouldBe` ( "MessageReceivedContext\n    { twilioMessageId = \""
                                    <> show (get #id twilioMessage)
                                    <> "\"\n    , twilioMessageSid = \"1234\"\n    , toPhoneNumber = \"+16666666666\"\n    , messageBody = \"Hi there!\"\n    }"
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
                    `shouldBe` ( "MessageProcessedContext\n    { twilioMessageId = \""
                                    <> show (get #id twilioMessage)
                                    <> "\"\n    , messageBody = \"Hi there!\"\n    , situation = \"SomeSituation\"\n    , plan = \"SomePlan\"\n    }"
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
                    `shouldBe` ( "MessageReceivedContext\n    { twilioMessageId = \""
                                    <> show (get #id twilioMessage)
                                    <> "\"\n    , twilioMessageSid = \"1234\"\n    , toPhoneNumber = \"+16666666666\"\n    , messageBody = \"Hi there!\"\n    }"
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
                    `shouldBe` "TimecardEntryContext\n    { timecardEntryId = 00000000 - 0000 - 0000 - 0000 - 000000000000\n    , date = 2021 - 10 - 30\n    , jobName = \"Costco\"\n    , clockedInAt = Just 07 : 00 : 00\n    , clockedOutAt = Just 07 : 00 : 01\n    , lunchDuration = Just 30\n    , hoursWorked = 8.0\n    , workDone = \"Ate chips.\"\n    , invoiceTranslation = \"Installed doors.\"\n    }"

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
                    `shouldBe` "TimecardEntryContext\n    { timecardEntryId = 00000000 - 0000 - 0000 - 0000 - 000000000000\n    , date = 2021 - 10 - 30\n    , jobName = \"Costco\"\n    , clockedInAt = Just 07 : 00 : 00\n    , clockedOutAt = Just 07 : 00 : 01\n    , lunchDuration = Just 30\n    , hoursWorked = 8.0\n    , workDone = \"Ate chips.\"\n    , invoiceTranslation = \"Installed doors.\"\n    }"

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
                    `shouldBe` "TimecardEntryContext\n    { timecardEntryId = 00000000 - 0000 - 0000 - 0000 - 000000000000\n    , date = 2021 - 10 - 30\n    , jobName = \"Costco\"\n    , clockedInAt = Just 07 : 00 : 00\n    , clockedOutAt = Just 07 : 00 : 01\n    , lunchDuration = Just 30\n    , hoursWorked = 8.0\n    , workDone = \"Ate chips.\"\n    , invoiceTranslation = \"Installed doors.\"\n    }"

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
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext\n    { sendMessageActionId = 10000000 - 0000 - 0000 - 0000 - 000000000000\n    , sendAt = 2021 - 10 - 30 14 : 00 : 00 UTC\n    , body = \"Yo!\"\n    }"

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
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext\n    { sendMessageActionId = 10000000 - 0000 - 0000 - 0000 - 000000000000\n    , sendAt = 2021 - 10 - 30 14 : 00 : 00 UTC\n    , body = \"Yo!\"\n    }"

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
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext\n    { sendMessageActionId = 10000000 - 0000 - 0000 - 0000 - 000000000000\n    , sendAt = 2021 - 10 - 30 14 : 00 : 00 UTC\n    , body = \"Yo!\"\n    }"

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
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext\n    { sendMessageActionId = 10000000 - 0000 - 0000 - 0000 - 000000000000\n    , sendAt = 2021 - 10 - 30 14 : 00 : 00 UTC\n    , body = \"Yo!\"\n    }"

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
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext\n    { sendMessageActionId = 10000000 - 0000 - 0000 - 0000 - 000000000000\n    , sendAt = 2021 - 10 - 30 14 : 00 : 00 UTC\n    , body = \"Yo!\"\n    }"

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
                get #actionContext auditEntry `shouldBe` "ScheduledMessageContext\n    { sendMessageActionId = 10000000 - 0000 - 0000 - 0000 - 000000000000\n    , sendAt = 2021 - 10 - 30 14 : 00 : 00 UTC\n    , body = \"Yo!\"\n    }"

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
