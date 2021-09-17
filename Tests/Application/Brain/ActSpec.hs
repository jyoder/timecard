module Tests.Application.Brain.ActSpec where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Brain.Act as Act
import qualified Application.Brain.Decide as Decide
import Generated.Types
import IHP.ControllerPrelude
import IHP.Test.Mocking
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "act" do
        beforeAll (testConfig >>= mockContext RootApplication) do
            context "when the plan says to create a timecard entry and schedule a reminder" do
                itIO "creates a timecard entry and schedules a reminder at the right time" do
                    bot <-
                        newRecord @Person
                            |> set #firstName "Tim"
                            |> set #lastName "Eckard"
                            |> set #goesBy "Tim the Bot"
                            |> createRecord

                    botPhoneNumber <-
                        newRecord @PhoneNumber
                            |> set #number "+14444444444"
                            |> createRecord

                    botPhoneContact <-
                        newRecord @PhoneContact
                            |> set #personId (get #id bot)
                            |> set #phoneNumberId (get #id botPhoneNumber)
                            |> createRecord

                    worker <-
                        newRecord @Person
                            |> set #firstName "Bill"
                            |> set #lastName "Gates"
                            |> set #goesBy "Billy"
                            |> createRecord

                    workerSettings <-
                        newRecord @WorkerSetting
                            |> set #personId (get #id worker)
                            |> set #sendDailyReminderAt (toTimeOfDay "15:30:00")
                            |> createRecord

                    workerPhoneNumber <-
                        newRecord @PhoneNumber
                            |> set #number "+15555555555"
                            |> createRecord

                    workerPhoneContact <-
                        newRecord @PhoneContact
                            |> set #personId (get #id worker)
                            |> set #phoneNumberId (get #id workerPhoneNumber)
                            |> createRecord

                    twilioMessage <-
                        newRecord @TwilioMessage
                            |> set #fromId (get #id workerPhoneNumber)
                            |> set #toId (get #id botPhoneNumber)
                            |> set #body "Let's do this."
                            |> createRecord

                    Act.act
                        Decide.CreateTimecardEntry
                            { now = toUtc "2021-08-30 15:30:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = get #id worker
                            , workerPhoneNumberId = get #id workerPhoneNumber
                            , botPhoneNumberId = get #id botPhoneNumber
                            , linkedMessageId = get #id twilioMessage
                            , date = toDay "2021-08-30"
                            , jobName = "123 Something Rd."
                            , hoursWorked = 7.5
                            , clockedInAt = Just $ toTimeOfDay "07:00:00"
                            , clockedOutAt = Just $ toTimeOfDay "15:30:00"
                            , lunchDuration = 30
                            , workDone = "some work done"
                            , invoiceTranslation = "some invoice translation"
                            }

                    timecard <-
                        query @Timecard
                            |> filterWhere (#personId, get #id worker)
                            |> fetchOne

                    timecardEntry <-
                        query @TimecardEntry
                            |> filterWhere (#timecardId, get #id timecard)
                            |> fetchOne

                    sendMessageAction <-
                        query @SendMessageAction
                            |> filterWhere (#toId, get #id workerPhoneNumber)
                            |> fetchOne

                    actionRunState <-
                        fetch (get #actionRunStateId sendMessageAction)

                    actionRunTime <-
                        query @ActionRunTime
                            |> filterWhere (#actionRunStateId, get #id actionRunState)
                            |> fetchOne

                    get #jobName timecardEntry `shouldBe` "123 Something Rd."
                    get #hoursWorked timecardEntry `shouldBe` 7.5
                    get #clockedInAt timecardEntry `shouldBe` Just (toTimeOfDay "07:00:00")
                    get #clockedOutAt timecardEntry `shouldBe` Just (toTimeOfDay "15:30:00")
                    get #lunchDuration timecardEntry `shouldBe` Just 30
                    get #workDone timecardEntry `shouldBe` "some work done"
                    get #invoiceTranslation timecardEntry `shouldBe` "some invoice translation"

                    get #body sendMessageAction `shouldBe` "Hey Billy - I've got you at 123 Something Rd. today. Let me know what hours you worked and what you did when you have a chance. Thanks!"
                    get #toId sendMessageAction `shouldBe` get #id workerPhoneNumber
                    get #fromId sendMessageAction `shouldBe` get #id botPhoneNumber
                    get #state actionRunState `shouldBe` ActionRunState.notStarted
                    get #runsAt actionRunTime `shouldBe` toUtc "2021-08-31 15:30:00 PDT"

            context "when the plan says to suspend an existing scheduled reminder" do
                itIO "suspend the existing scheduled reminder" do
                    actionRunState <-
                        newRecord @ActionRunState
                            |> set #state ActionRunState.notStarted
                            |> createRecord

                    Act.act
                        Decide.SuspendReminder
                            { actionRunStateId = get #id actionRunState
                            }

                    actionRunState <- fetch $ get #id actionRunState
                    get #state actionRunState `shouldBe` ActionRunState.suspended

            context "when the plan says to do nothing" do
                itIO "does nothing" do
                    Act.act Decide.DoNothing
