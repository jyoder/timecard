module Tests.Application.Twilio.QuerySpec where

import Application.Service.Transaction (withTransactionOrSavepoint)
import qualified Application.Twilio.Query as Query
import qualified Application.Twilio.TwilioMessage as TwilioMessage
import Data.Set (elems)
import "string-interpolate" Data.String.Interpolate (i)
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "fetchById" do
            itIO "selects only the message with the given id" do
                bob <-
                    newRecord @Person
                        |> set #firstName "Bob"
                        |> set #lastName "Bobbers"
                        |> set #goesBy "Bob"
                        |> createRecord

                bobPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+12222222222"
                        |> createRecord

                bobContact <-
                    newRecord @PhoneContact
                        |> set #personId (get #id bob)
                        |> set #phoneNumberId (get #id bobPhoneNumber)
                        |> createRecord

                alice <-
                    newRecord @Person
                        |> set #firstName "Alice"
                        |> set #lastName "Allers"
                        |> set #goesBy "Al"
                        |> createRecord

                alicePhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+13333333333"
                        |> createRecord

                aliceContact <-
                    newRecord @PhoneContact
                        |> set #personId (get #id alice)
                        |> set #phoneNumberId (get #id alicePhoneNumber)
                        |> createRecord

                bobToAliceMessage1 <-
                    newRecord @TwilioMessage
                        |> set #createdAt (toUtc "2021-08-24 00:00:00 PDT")
                        |> set #fromId (get #id bobPhoneNumber)
                        |> set #toId (get #id alicePhoneNumber)
                        |> set #body "Hi Alice!"
                        |> set #status TwilioMessage.delivered
                        |> set #messageSid "1"
                        |> createRecord

                bobToAliceMessage2 <-
                    newRecord @TwilioMessage
                        |> set #createdAt (toUtc "2021-08-25 00:00:00 PDT")
                        |> set #fromId (get #id bobPhoneNumber)
                        |> set #toId (get #id alicePhoneNumber)
                        |> set #body "Hi Alice again!"
                        |> set #status TwilioMessage.delivered
                        |> set #messageSid "2"
                        |> createRecord

                rows <- Query.fetchById $ get #id bobToAliceMessage2

                rows
                    `shouldBe` [ Query.Row
                                    { id = get #id bobToAliceMessage2
                                    , fromPhoneNumber = "+12222222222"
                                    , fromFirstName = "Bob"
                                    , fromLastName = "Bobbers"
                                    , toPhoneNumber = "+13333333333"
                                    , toFirstName = "Alice"
                                    , toLastName = "Allers"
                                    , createdAt = toUtc "2021-08-25 00:00:00 PDT"
                                    , status = Query.Delivered
                                    , body = Just "Hi Alice again!"
                                    , entityType = Nothing
                                    , entityStart = Nothing
                                    , entityEnd = Nothing
                                    , entityConfidence = Nothing
                                    }
                               ]

            itIO "includes entity prediction information if present, ordered by segment start" do
                bob <-
                    newRecord @Person
                        |> set #firstName "Bob"
                        |> set #lastName "Bobbers"
                        |> set #goesBy "Bob"
                        |> createRecord

                bobPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+12222222222"
                        |> createRecord

                bobContact <-
                    newRecord @PhoneContact
                        |> set #personId (get #id bob)
                        |> set #phoneNumberId (get #id bobPhoneNumber)
                        |> createRecord

                alice <-
                    newRecord @Person
                        |> set #firstName "Alice"
                        |> set #lastName "Allers"
                        |> set #goesBy "Al"
                        |> createRecord

                alicePhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+13333333333"
                        |> createRecord

                aliceContact <-
                    newRecord @PhoneContact
                        |> set #personId (get #id alice)
                        |> set #phoneNumberId (get #id alicePhoneNumber)
                        |> createRecord

                bobToAliceMessage <-
                    newRecord @TwilioMessage
                        |> set #fromId (get #id bobPhoneNumber)
                        |> set #toId (get #id alicePhoneNumber)
                        |> set #body "Hi Alice!"
                        |> set #status TwilioMessage.delivered
                        |> set #messageSid "1"
                        |> createRecord

                prediction1 <-
                    newRecord @VertexAiEntityPrediction
                        |> set #displayName "unrecognized"
                        |> set #segmentStartOffset 3
                        |> set #segmentEndOffset 5
                        |> set #confidence 0.9
                        |> set #vertexAiId "a"
                        |> createRecord

                newRecord @TwilioMessageEntity
                    |> set #twilioMessageId (get #id bobToAliceMessage)
                    |> set #vertexAiEntityPredictionId (get #id prediction1)
                    |> createRecord

                prediction2 <-
                    newRecord @VertexAiEntityPrediction
                        |> set #displayName "unrecognized"
                        |> set #segmentStartOffset 0
                        |> set #segmentEndOffset 2
                        |> set #confidence 0.8
                        |> set #vertexAiId "b"
                        |> createRecord

                newRecord @TwilioMessageEntity
                    |> set #twilioMessageId (get #id bobToAliceMessage)
                    |> set #vertexAiEntityPredictionId (get #id prediction2)
                    |> createRecord

                rows <- Query.fetchById $ get #id bobToAliceMessage

                rows
                    `shouldBe` [ Query.Row
                                    { id = get #id bobToAliceMessage
                                    , fromPhoneNumber = "+12222222222"
                                    , fromFirstName = "Bob"
                                    , fromLastName = "Bobbers"
                                    , toPhoneNumber = "+13333333333"
                                    , toFirstName = "Alice"
                                    , toLastName = "Allers"
                                    , createdAt = get #createdAt bobToAliceMessage
                                    , status = Query.Delivered
                                    , body = Just "Hi Alice!"
                                    , entityType = Just Query.Unrecognized
                                    , entityStart = Just 0
                                    , entityEnd = Just 2
                                    , entityConfidence = Just 0.8
                                    }
                               , Query.Row
                                    { id = get #id bobToAliceMessage
                                    , fromPhoneNumber = "+12222222222"
                                    , fromFirstName = "Bob"
                                    , fromLastName = "Bobbers"
                                    , toPhoneNumber = "+13333333333"
                                    , toFirstName = "Alice"
                                    , toLastName = "Allers"
                                    , createdAt = get #createdAt bobToAliceMessage
                                    , status = Query.Delivered
                                    , body = Nothing
                                    , entityType = Just Query.Unrecognized
                                    , entityStart = Just 3
                                    , entityEnd = Just 5
                                    , entityConfidence = Just 0.9
                                    }
                               ]

            itIO "tracks the correct set of tables" do
                withTableReadTracker do
                    Query.fetchById "10000000-0000-0000-0000-000000000000"
                    trackedTables <- readIORef ?touchedTables
                    elems trackedTables
                        `shouldBe` [ "twilio_message_entities"
                                   , "twilio_messages"
                                   ]

        describe "fetchByPeople" do
            itIO "selects only messages between the two given people" do
                bob <-
                    newRecord @Person
                        |> set #firstName "Bob"
                        |> set #lastName "Bobbers"
                        |> set #goesBy "Bob"
                        |> createRecord

                bobPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+12222222222"
                        |> createRecord

                bobContact <-
                    newRecord @PhoneContact
                        |> set #personId (get #id bob)
                        |> set #phoneNumberId (get #id bobPhoneNumber)
                        |> createRecord

                alice <-
                    newRecord @Person
                        |> set #firstName "Alice"
                        |> set #lastName "Allers"
                        |> set #goesBy "Al"
                        |> createRecord

                alicePhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+13333333333"
                        |> createRecord

                aliceContact <-
                    newRecord @PhoneContact
                        |> set #personId (get #id alice)
                        |> set #phoneNumberId (get #id alicePhoneNumber)
                        |> createRecord

                bill <-
                    newRecord @Person
                        |> set #firstName "Bill"
                        |> set #lastName "Billies"
                        |> set #goesBy "Biller"
                        |> createRecord

                billPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                billContact <-
                    newRecord @PhoneContact
                        |> set #personId (get #id bill)
                        |> set #phoneNumberId (get #id billPhoneNumber)
                        |> createRecord

                bobToAliceMessage <-
                    newRecord @TwilioMessage
                        |> set #createdAt (toUtc "2021-08-24 00:00:00 PDT")
                        |> set #fromId (get #id bobPhoneNumber)
                        |> set #toId (get #id alicePhoneNumber)
                        |> set #body "Hi Alice!"
                        |> set #status TwilioMessage.delivered
                        |> set #messageSid "1"
                        |> createRecord

                bobToBillMessage <-
                    newRecord @TwilioMessage
                        |> set #fromId (get #id bobPhoneNumber)
                        |> set #toId (get #id billPhoneNumber)
                        |> set #body "Hi Bill!"
                        |> set #status TwilioMessage.delivered
                        |> set #messageSid "2"
                        |> createRecord

                rows <- Query.fetchByPeople (get #id bob) (get #id alice)

                rows
                    `shouldBe` [ Query.Row
                                    { id = get #id bobToAliceMessage
                                    , fromPhoneNumber = "+12222222222"
                                    , fromFirstName = "Bob"
                                    , fromLastName = "Bobbers"
                                    , toPhoneNumber = "+13333333333"
                                    , toFirstName = "Alice"
                                    , toLastName = "Allers"
                                    , createdAt = toUtc "2021-08-24 00:00:00 PDT"
                                    , status = Query.Delivered
                                    , body = Just "Hi Alice!"
                                    , entityType = Nothing
                                    , entityStart = Nothing
                                    , entityEnd = Nothing
                                    , entityConfidence = Nothing
                                    }
                               ]

            itIO "returns messages in ascending order based on creation time" do
                bob <-
                    newRecord @Person
                        |> set #firstName "Bob"
                        |> set #lastName "Bobbers"
                        |> set #goesBy "Bob"
                        |> createRecord

                bobPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+12222222222"
                        |> createRecord

                bobContact <-
                    newRecord @PhoneContact
                        |> set #personId (get #id bob)
                        |> set #phoneNumberId (get #id bobPhoneNumber)
                        |> createRecord

                alice <-
                    newRecord @Person
                        |> set #firstName "Alice"
                        |> set #lastName "Allers"
                        |> set #goesBy "Al"
                        |> createRecord

                alicePhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+13333333333"
                        |> createRecord

                aliceContact <-
                    newRecord @PhoneContact
                        |> set #personId (get #id alice)
                        |> set #phoneNumberId (get #id alicePhoneNumber)
                        |> createRecord

                bobToAliceMessage1 <-
                    newRecord @TwilioMessage
                        |> set #createdAt (toUtc "2021-08-24 00:00:00 PDT")
                        |> set #fromId (get #id bobPhoneNumber)
                        |> set #toId (get #id alicePhoneNumber)
                        |> set #body "Hi Alice!"
                        |> set #status TwilioMessage.delivered
                        |> set #messageSid "1"
                        |> createRecord

                bobToAliceMessage2 <-
                    newRecord @TwilioMessage
                        |> set #createdAt (toUtc "2021-08-25 00:00:00 PDT")
                        |> set #fromId (get #id bobPhoneNumber)
                        |> set #toId (get #id alicePhoneNumber)
                        |> set #body "Hi Alice again!"
                        |> set #status TwilioMessage.delivered
                        |> set #messageSid "2"
                        |> createRecord

                rows <- Query.fetchByPeople (get #id bob) (get #id alice)

                rows
                    `shouldBe` [ Query.Row
                                    { id = get #id bobToAliceMessage1
                                    , fromPhoneNumber = "+12222222222"
                                    , fromFirstName = "Bob"
                                    , fromLastName = "Bobbers"
                                    , toPhoneNumber = "+13333333333"
                                    , toFirstName = "Alice"
                                    , toLastName = "Allers"
                                    , createdAt = toUtc "2021-08-24 00:00:00 PDT"
                                    , status = Query.Delivered
                                    , body = Just "Hi Alice!"
                                    , entityType = Nothing
                                    , entityStart = Nothing
                                    , entityEnd = Nothing
                                    , entityConfidence = Nothing
                                    }
                               , Query.Row
                                    { id = get #id bobToAliceMessage2
                                    , fromPhoneNumber = "+12222222222"
                                    , fromFirstName = "Bob"
                                    , fromLastName = "Bobbers"
                                    , toPhoneNumber = "+13333333333"
                                    , toFirstName = "Alice"
                                    , toLastName = "Allers"
                                    , createdAt = toUtc "2021-08-25 00:00:00 PDT"
                                    , status = Query.Delivered
                                    , body = Just "Hi Alice again!"
                                    , entityType = Nothing
                                    , entityStart = Nothing
                                    , entityEnd = Nothing
                                    , entityConfidence = Nothing
                                    }
                               ]

            itIO "limits number of messages returned to 60 (about one month's worth of communication)" do
                bob <-
                    newRecord @Person
                        |> set #firstName "Bob"
                        |> set #lastName "Bobbers"
                        |> set #goesBy "Bob"
                        |> createRecord

                bobPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+12222222222"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id bob)
                    |> set #phoneNumberId (get #id bobPhoneNumber)
                    |> createRecord

                alice <-
                    newRecord @Person
                        |> set #firstName "Alice"
                        |> set #lastName "Allers"
                        |> set #goesBy "Al"
                        |> createRecord

                alicePhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+13333333333"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #personId (get #id alice)
                    |> set #phoneNumberId (get #id alicePhoneNumber)
                    |> createRecord

                let alicePhoneNumberId = show $ get #id alicePhoneNumber
                    bobPhoneNumberId = show $ get #id bobPhoneNumber
                    insertTuples = map (\sid -> [i| ('2010-04-01', #{show sid}, '1', NULL, '#{alicePhoneNumberId}', '#{bobPhoneNumberId}', 'delivered', 'Test', 0) |]) [1 .. 61]
                    insertStatement =
                        [i|
                            insert into 
                                public.twilio_messages 
                                    (api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) 
                                values 
                                    #{intercalate ",\n" insertTuples};
                        |]

                sqlExec insertStatement ()

                message <- query @TwilioMessage |> fetchOne
                prediction1 <-
                    newRecord @VertexAiEntityPrediction
                        |> set #displayName "unrecognized"
                        |> set #segmentStartOffset 1
                        |> set #segmentEndOffset 2
                        |> set #confidence 0.9
                        |> set #vertexAiId "a"
                        |> createRecord

                newRecord @TwilioMessageEntity
                    |> set #twilioMessageId (get #id message)
                    |> set #vertexAiEntityPredictionId (get #id prediction1)
                    |> createRecord

                prediction2 <-
                    newRecord @VertexAiEntityPrediction
                        |> set #displayName "unrecognized"
                        |> set #segmentStartOffset 3
                        |> set #segmentEndOffset 5
                        |> set #confidence 0.9
                        |> set #vertexAiId "b"
                        |> createRecord

                newRecord @TwilioMessageEntity
                    |> set #twilioMessageId (get #id message)
                    |> set #vertexAiEntityPredictionId (get #id prediction2)
                    |> createRecord

                rows <- Query.fetchByPeople (get #id bob) (get #id alice)

                -- Should be 61 since the presence of two predictions for a message increases
                -- the number of rows.
                length rows `shouldBe` 61

        itIO "tracks the correct set of tables" do
            withTableReadTracker do
                Query.fetchByPeople
                    "10000000-0000-0000-0000-000000000000"
                    "20000000-0000-0000-0000-000000000000"
                trackedTables <- readIORef ?touchedTables
                elems trackedTables
                    `shouldBe` [ "twilio_message_entities"
                               , "twilio_messages"
                               ]