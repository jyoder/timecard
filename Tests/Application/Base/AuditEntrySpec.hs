module Tests.Application.Base.AuditEntrySpec where

import Application.Base.AuditEntry
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "messageSentContext" do
        it "returns a message sent context" do
            let twilioMessage =
                    newRecord @TwilioMessage
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #messageSid "1234"
                        |> set #body "Hi there!"
             in messageSentContext twilioMessage "+15555555555"
                    `shouldBe` MessageSentContext
                        { twilioMessageId = "10000000-0000-0000-0000-000000000000"
                        , twilioMessageSid = "1234"
                        , fromPhoneNumber = "+15555555555"
                        , messageBody = "Hi there!"
                        }

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
                    `shouldBe` ( "MessageSentContext {twilioMessageId = \""
                                    <> show (get #id twilioMessage)
                                    <> "\", twilioMessageSid = \"1234\", fromPhoneNumber = \"+15555555555\", messageBody = \"Hi there!\"}"
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
