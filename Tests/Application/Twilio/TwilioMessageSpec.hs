module Tests.Application.Twilio.TwilioMessageSpec where

import qualified Application.Twilio.TwilioMessage as TwilioMessage
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "validate" do
        it "validates all fields" do
            let twilioMessage =
                    newRecord @TwilioMessage
                        |> set #apiVersion ""
                        |> set #messageSid ""
                        |> set #accountSid ""
                        |> set #status "unknown"
                        |> set #body ""
                        |> set #numMedia (-1)

            let twilioMessage' = TwilioMessage.validate twilioMessage
            twilioMessage' |> get #meta |> get #annotations
                `shouldBe` [ ("numMedia", TextViolation {message = "did not pass any validators"})
                           , ("body", TextViolation {message = "This field cannot be empty"})
                           , ("status", TextViolation {message = "is not allowed. It needs to be one of the following: [\"accepted\",\"scheduled\",\"queued\",\"sending\",\"sent\",\"receiving\",\"received\",\"delivered\",\"undelivered\",\"failed\",\"read\"]"})
                           , ("accountSid", TextViolation {message = "This field cannot be empty"})
                           , ("messageSid", TextViolation {message = "This field cannot be empty"})
                           , ("apiVersion", TextViolation {message = "This field cannot be empty"})
                           ]

    aroundAll (withApp RootApplication testConfig) do
        describe "send" do
            itIO "sends a twilio message and records an audit entry" do
                user <-
                    newRecord @User
                        |> set #email "test@user.com"
                        |> set #passwordHash "some password"
                        |> createRecord

                fromPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                toPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                auditEntryCount <-
                    query @AuditEntry
                        |> filterWhere (#userId, Just $ get #id user)
                        |> filterWhere (#action, MessageSent)
                        |> fetchCount

                twilioMessage <-
                    TwilioMessage.send
                        (Just $ get #id user)
                        fromPhoneNumber
                        toPhoneNumber
                        "Hi there!"

                get #apiVersion twilioMessage `shouldBe` "fakeApiVersion"
                get #accountSid twilioMessage `shouldBe` "fakeAccountSid"
                get #messagingServiceSid twilioMessage `shouldBe` Nothing
                get #fromId twilioMessage `shouldBe` get #id fromPhoneNumber
                get #toId twilioMessage `shouldBe` get #id toPhoneNumber
                get #body twilioMessage `shouldBe` "Hi there!"

                auditEntryCount' <-
                    query @AuditEntry
                        |> filterWhere (#userId, Just $ get #id user)
                        |> filterWhere (#phoneNumberId, get #toId twilioMessage)
                        |> filterWhere (#action, MessageSent)
                        |> fetchCount
                auditEntryCount' `shouldBe` auditEntryCount + 1
