module Tests.Application.Brain.ActSpec where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Brain.Act as Act
import qualified Application.Brain.Decide as Decide
import qualified Application.Timecard.Entry as Timecard.Entry
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    aroundAll (withApp RootApplication testConfig) do
        describe "act" do
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
                        "https://timecard.company.com"
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
                    get #runsAt actionRunState `shouldBe` toUtc "2021-08-31 15:30:00 PDT"

                itIO "creates a timecard entry, schedules a reminder, and schedules a review if the timecard is complete" do
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

                    timecardEntryMonday <-
                        newRecord @TimecardEntry
                            |> set #date (toDay "2021-08-30")
                            |> set #jobName "McDonald's"
                            |> set #hoursWorked 8.0
                            |> set #workDone "work"
                            |> set #invoiceTranslation "invoice"
                            |> Timecard.Entry.create
                                Nothing
                                (get #id worker)
                                (get #id workerPhoneNumber)
                                []

                    timecardEntryTuesday <-
                        newRecord @TimecardEntry
                            |> set #date (toDay "2021-08-31")
                            |> set #jobName "McDonald's"
                            |> set #hoursWorked 8.0
                            |> set #workDone "work"
                            |> set #invoiceTranslation "invoice"
                            |> Timecard.Entry.create
                                Nothing
                                (get #id worker)
                                (get #id workerPhoneNumber)
                                []

                    timecardEntryWednesday <-
                        newRecord @TimecardEntry
                            |> set #date (toDay "2021-09-01")
                            |> set #jobName "McDonald's"
                            |> set #hoursWorked 8.0
                            |> set #workDone "work"
                            |> set #invoiceTranslation "invoice"
                            |> Timecard.Entry.create
                                Nothing
                                (get #id worker)
                                (get #id workerPhoneNumber)
                                []

                    timecardEntryThursday <-
                        newRecord @TimecardEntry
                            |> set #date (toDay "2021-09-02")
                            |> set #jobName "McDonald's"
                            |> set #hoursWorked 8.0
                            |> set #workDone "work"
                            |> set #invoiceTranslation "invoice"
                            |> Timecard.Entry.create
                                Nothing
                                (get #id worker)
                                (get #id workerPhoneNumber)
                                []

                    twilioMessage <-
                        newRecord @TwilioMessage
                            |> set #fromId (get #id workerPhoneNumber)
                            |> set #toId (get #id botPhoneNumber)
                            |> set #body "Let's do this."
                            |> createRecord

                    Act.act
                        "https://timecard.company.com"
                        Decide.CreateTimecardEntry
                            { now = toUtc "2021-09-03 15:30:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = get #id worker
                            , workerPhoneNumberId = get #id workerPhoneNumber
                            , botPhoneNumberId = get #id botPhoneNumber
                            , linkedMessageId = get #id twilioMessage
                            , date = toDay "2021-09-03"
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

                    timecardEntryFriday <-
                        query @TimecardEntry
                            |> filterWhere (#date, toDay "2021-09-03")
                            |> fetchOne

                    entryActionRunState <-
                        query @ActionRunState
                            |> filterWhere (#runsAt, toUtc "2021-09-06 15:30:00 PDT")
                            |> fetchOne

                    entryRequest <-
                        query @SendMessageAction
                            |> filterWhere (#toId, get #id workerPhoneNumber)
                            |> filterWhere (#actionRunStateId, get #id entryActionRunState)
                            |> fetchOne

                    entryActionRunState <-
                        fetch (get #actionRunStateId entryRequest)

                    reviewActionRunState <-
                        query @ActionRunState
                            |> filterWhere (#runsAt, toUtc "2021-09-03 15:34:00 PDT")
                            |> fetchOne

                    reviewRequest <-
                        query @SendMessageAction
                            |> filterWhere (#toId, get #id workerPhoneNumber)
                            |> filterWhere (#actionRunStateId, get #id reviewActionRunState)
                            |> fetchOne

                    reviewActionRunState <-
                        fetch (get #actionRunStateId reviewRequest)

                    get #jobName timecardEntryFriday `shouldBe` "123 Something Rd."
                    get #hoursWorked timecardEntryFriday `shouldBe` 7.5
                    get #clockedInAt timecardEntryFriday `shouldBe` Just (toTimeOfDay "07:00:00")
                    get #clockedOutAt timecardEntryFriday `shouldBe` Just (toTimeOfDay "15:30:00")
                    get #lunchDuration timecardEntryFriday `shouldBe` Just 30
                    get #workDone timecardEntryFriday `shouldBe` "some work done"
                    get #invoiceTranslation timecardEntryFriday `shouldBe` "some invoice translation"

                    get #body entryRequest `shouldBe` "Hey Billy - I've got you at 123 Something Rd. today. Let me know what hours you worked and what you did when you have a chance. Thanks!"
                    get #toId entryRequest `shouldBe` get #id workerPhoneNumber
                    get #fromId entryRequest `shouldBe` get #id botPhoneNumber
                    get #state entryActionRunState `shouldBe` ActionRunState.notStarted

                    get #body reviewRequest `shouldSatisfy` isPrefixOf "Thanks Billy. Here's your timecard to review and sign"
                    get #toId reviewRequest `shouldBe` get #id workerPhoneNumber
                    get #fromId reviewRequest `shouldBe` get #id botPhoneNumber
                    get #state reviewActionRunState `shouldBe` ActionRunState.notStarted

            context "when the plan says to suspend an existing scheduled reminder" do
                itIO "suspends scheduled messages" do
                    botPhoneNumber <-
                        newRecord @PhoneNumber
                            |> set #number "+14444444444"
                            |> createRecord

                    workerPhoneNumber <-
                        newRecord @PhoneNumber
                            |> set #number "+15555555555"
                            |> createRecord

                    actionRunState1 <-
                        newRecord @ActionRunState
                            |> set #state ActionRunState.notStarted
                            |> createRecord

                    sendMessageAction1 <-
                        newRecord @SendMessageAction
                            |> set #actionRunStateId (get #id actionRunState1)
                            |> set #fromId (get #id botPhoneNumber)
                            |> set #toId (get #id workerPhoneNumber)
                            |> set #body "1st message"
                            |> createRecord

                    actionRunState2 <-
                        newRecord @ActionRunState
                            |> set #state ActionRunState.notStarted
                            |> createRecord

                    sendMessageAction2 <-
                        newRecord @SendMessageAction
                            |> set #actionRunStateId (get #id actionRunState2)
                            |> set #fromId (get #id botPhoneNumber)
                            |> set #toId (get #id workerPhoneNumber)
                            |> set #body "2nd message"
                            |> createRecord

                    auditEntryCount <-
                        query @AuditEntry
                            |> filterWhere (#phoneNumberId, get #id workerPhoneNumber)
                            |> filterWhere (#action, ScheduledMessageSuspended)
                            |> fetchCount

                    Act.act
                        "https://timecard.company.com"
                        Decide.SuspendScheduledMessages
                            { actionRunStateIds =
                                [ get #id actionRunState1
                                , get #id actionRunState2
                                ]
                            }

                    actionRunState1 <- fetch $ get #id actionRunState1
                    get #state actionRunState1 `shouldBe` ActionRunState.suspended

                    actionRunState2 <- fetch $ get #id actionRunState2
                    get #state actionRunState2 `shouldBe` ActionRunState.suspended

                    auditEntryCount' <-
                        query @AuditEntry
                            |> filterWhere (#phoneNumberId, get #id workerPhoneNumber)
                            |> filterWhere (#action, ScheduledMessageSuspended)
                            |> fetchCount
                    auditEntryCount' `shouldBe` auditEntryCount + 2

            context "when the plan says to do nothing" do
                itIO "does nothing" do
                    Act.act "https://timecard.company.com" Decide.DoNothing
