module Tests.Application.Brain.ObserveSpec where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Brain.Observe as Observe
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Twilio.Query as Twilio.Query
import qualified Application.Twilio.View as Twilio.View
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support
import Text.Read (read)

spec :: Spec
spec = do
    describe "observe" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            itIO "loads all required context and returns a set of observations" do
                now' <- getCurrentTime

                worker <-
                    newRecord @Person
                        |> set #firstName "Ronald"
                        |> set #lastName "McDonald"
                        |> set #goesBy "Ron"
                        |> createRecord

                bot <-
                    newRecord @Person
                        |> set #firstName "Tim"
                        |> set #lastName "Eckard"
                        |> set #goesBy "Tim the Bot"
                        |> createRecord

                timecard <-
                    newRecord @Timecard
                        |> set #weekOf (toDay "2021-06-21")
                        |> set #personId (get #id worker)
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

                actionRunState <-
                    newRecord @ActionRunState
                        |> set #state ActionRunState.notStarted
                        |> createRecord

                actionRunTime <-
                    newRecord @ActionRunTime
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #runsAt (toUtc "2021-06-23 15:00:00 PDT")
                        |> createRecord

                botPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+14444444444"
                        |> createRecord

                workerPhoneNumber <-
                    newRecord @PhoneNumber
                        |> set #number "+15555555555"
                        |> createRecord

                newRecord @PhoneContact
                    |> set #phoneNumberId (get #id botPhoneNumber)
                    |> set #personId (get #id bot)
                    |> createRecord

                newRecord @PhoneContact
                    |> set #phoneNumberId (get #id workerPhoneNumber)
                    |> set #personId (get #id worker)
                    |> createRecord

                sendMessageAction <-
                    newRecord @SendMessageAction
                        |> set #actionRunStateId (get #id actionRunState)
                        |> set #fromId (get #id botPhoneNumber)
                        |> set #toId (get #id workerPhoneNumber)
                        |> set #body "Hello!"
                        |> createRecord

                observations <-
                    Observe.observe
                        Observe.IncomingMessage
                            { message =
                                Twilio.View.Message
                                    { id = "10000000-0000-0000-0000-000000000000"
                                    , fromPhoneNumber = "+5555555555"
                                    , fromFirstName = "John"
                                    , fromLastName = "Jingles"
                                    , toPhoneNumber = "+6666666666"
                                    , toFirstName = "Jing"
                                    , toLastName = "Jongles"
                                    , createdAt = toUtc "2021-08-20 13:00:00 PDT"
                                    , status = Twilio.Query.Received
                                    , body = "I cut toothpicks for 8hrs at 123 Happy Place."
                                    , entities = []
                                    }
                            , workerId = get #id worker
                            , workerPhoneNumberId = get #id workerPhoneNumber
                            , botPhoneNumberId = get #id botPhoneNumber
                            }

                let Observe.Observations {..} = observations
                now' `shouldSatisfy` (>=) now
                companyTimeZone `shouldBe` toTimeZone "PDT"
                today `shouldSatisfy` (>=) (localDay (utcToLocalTime (toTimeZone "PDT") now'))

                event
                    `shouldBe` Observe.IncomingMessage
                        { message =
                            Twilio.View.Message
                                { id = "10000000-0000-0000-0000-000000000000"
                                , fromPhoneNumber = "+5555555555"
                                , fromFirstName = "John"
                                , fromLastName = "Jingles"
                                , toPhoneNumber = "+6666666666"
                                , toFirstName = "Jing"
                                , toLastName = "Jongles"
                                , createdAt = toUtc "2021-08-20 13:00:00 PDT"
                                , status = Twilio.Query.Received
                                , body = "I cut toothpicks for 8hrs at 123 Happy Place."
                                , entities = []
                                }
                        , workerId = get #id worker
                        , workerPhoneNumberId = get #id workerPhoneNumber
                        , botPhoneNumberId = get #id botPhoneNumber
                        }

                timecardEntryRows
                    `shouldBe` [ Timecard.Query.Row
                                    { timecardId = get #id timecard
                                    , timecardPersonId = get #id worker
                                    , timecardWeekOf = toDay "2021-06-21"
                                    , accessTokenId = Nothing
                                    , accessTokenValue = Nothing
                                    , accessTokenExpiresAt = Nothing
                                    , accessTokenIsRevoked = Nothing
                                    , signingId = Nothing
                                    , signingSignedAt = Nothing
                                    , timecardEntryId = get #id timecardEntry
                                    , timecardEntryDate = toDay "2021-06-23"
                                    , timecardEntryJobName = "McDonald's"
                                    , timecardEntryClockedInAt = Nothing
                                    , timecardEntryClockedOutAt = Nothing
                                    , timecardEntryLunchDuration = Nothing
                                    , timecardEntryHoursWorked = 8.0
                                    , timecardEntryWorkDone = "work"
                                    , timecardEntryInvoiceTranslation = "invoice"
                                    }
                               ]

                scheduledReminders
                    `shouldBe` [ SendMessageAction.T
                                    { id = get #id sendMessageAction
                                    , actionRunStateId = get #id actionRunState
                                    , state = ActionRunState.notStarted
                                    , runsAt = toUtc "2021-06-23 15:00:00 PDT"
                                    , body = "Hello!"
                                    , fromId = get #id botPhoneNumber
                                    , fromNumber = "+14444444444"
                                    , toId = get #id workerPhoneNumber
                                    , toNumber = "+15555555555"
                                    }
                               ]