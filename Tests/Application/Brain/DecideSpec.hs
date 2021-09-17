module Tests.Application.Brain.DecideSpec where

import qualified Application.Brain.Decide as Decide
import qualified Application.Brain.Orient as Orient
import IHP.Prelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "decide" do
        context "when the situation does not have a scheduled reminder" do
            context "and the update is for a single job" do
                it "returns a plan to create a timecard entry and schedule a reminder" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update =
                                Orient.UpdateIsForASingleJob
                                    Orient.Job
                                        { date = toDay "2021-08-30"
                                        , name = "123 Some job"
                                        , clockedInAt = Just $ toTimeOfDay "07:30:00"
                                        , clockedOutAt = Just $ toTimeOfDay "15:30:00"
                                        , lunchDuration = 30
                                        , hoursWorked = 8.0
                                        , workDone = "Saw some boards."
                                        , invoiceTranslation = "Trim some boards."
                                        }
                            , reminder = Orient.ReminderIsNotScheduled
                            }
                        `shouldBe` Decide.CreateTimecardEntry
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , linkedMessageId = "40000000-0000-0000-0000-000000000000"
                            , date = toDay "2021-08-30"
                            , jobName = "123 Some job"
                            , hoursWorked = 8.0
                            , clockedInAt = Just $ toTimeOfDay "07:30:00"
                            , clockedOutAt = Just $ toTimeOfDay "15:30:00"
                            , lunchDuration = 30
                            , workDone = "Saw some boards."
                            , invoiceTranslation = "Trim some boards."
                            }

            context "and the update is for multiple jobs" do
                it "returns a plan to do nothing" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update = Orient.UpdateIsForMultipleJobs
                            , reminder = Orient.ReminderIsNotScheduled
                            }
                        `shouldBe` Decide.DoNothing

            context "and the details don't match in the update" do
                it "returns a plan to do nothing" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update = Orient.UpdateDetailsDoNotMatch
                            , reminder = Orient.ReminderIsNotScheduled
                            }
                        `shouldBe` Decide.DoNothing

            context "and the message is not an update" do
                it "returns a plan to do nothing" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update = Orient.MessageIsNotAnUpdate
                            , reminder = Orient.ReminderIsNotScheduled
                            }
                        `shouldBe` Decide.DoNothing

        context "when the situation does have a scheduled reminder" do
            context "and the update is for a single job" do
                it "returns a plan to suspend scheduled messages" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update =
                                Orient.UpdateIsForASingleJob
                                    Orient.Job
                                        { date = toDay "2021-08-30"
                                        , name = "123 Some job"
                                        , clockedInAt = Just $ toTimeOfDay "07:30:00"
                                        , clockedOutAt = Just $ toTimeOfDay "15:30:00"
                                        , lunchDuration = 30
                                        , hoursWorked = 8.0
                                        , workDone = "Saw some boards."
                                        , invoiceTranslation = "Trim some boards."
                                        }
                            , reminder =
                                Orient.ReminderIsScheduled
                                    { actionRunStateId = "50000000-0000-0000-0000-000000000000"
                                    }
                            }
                        `shouldBe` Decide.SuspendScheduledMessages
                            { actionRunStateId = "50000000-0000-0000-0000-000000000000"
                            }

            context "and the update is for multiple jobs" do
                it "returns a plan to suspend scheduled messages" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update = Orient.UpdateIsForMultipleJobs
                            , reminder =
                                Orient.ReminderIsScheduled
                                    { actionRunStateId = "50000000-0000-0000-0000-000000000000"
                                    }
                            }
                        `shouldBe` Decide.SuspendScheduledMessages
                            { actionRunStateId = "50000000-0000-0000-0000-000000000000"
                            }

            context "and the details don't match in the update" do
                it "returns a plan to suspend scheduled messages" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update = Orient.UpdateDetailsDoNotMatch
                            , reminder =
                                Orient.ReminderIsScheduled
                                    { actionRunStateId = "50000000-0000-0000-0000-000000000000"
                                    }
                            }
                        `shouldBe` Decide.SuspendScheduledMessages
                            { actionRunStateId = "50000000-0000-0000-0000-000000000000"
                            }

            context "and the message is not an update" do
                it "returns a plan to suspend scheduled messages" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update = Orient.MessageIsNotAnUpdate
                            , reminder =
                                Orient.ReminderIsScheduled
                                    { actionRunStateId = "50000000-0000-0000-0000-000000000000"
                                    }
                            }
                        `shouldBe` Decide.SuspendScheduledMessages
                            { actionRunStateId = "50000000-0000-0000-0000-000000000000"
                            }

        context "when the situation has a suspended reminder" do
            context "and the update is for a single job" do
                it "returns a plan to do nothing" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update =
                                Orient.UpdateIsForASingleJob
                                    Orient.Job
                                        { date = toDay "2021-08-30"
                                        , name = "123 Some job"
                                        , clockedInAt = Just $ toTimeOfDay "07:30:00"
                                        , clockedOutAt = Just $ toTimeOfDay "15:30:00"
                                        , lunchDuration = 30
                                        , hoursWorked = 8.0
                                        , workDone = "Saw some boards."
                                        , invoiceTranslation = "Trim some boards."
                                        }
                            , reminder = Orient.ReminderIsSuspended
                            }
                        `shouldBe` Decide.DoNothing

            context "and the update is for multiple jobs" do
                it "returns a plan to do nothing" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update = Orient.UpdateIsForMultipleJobs
                            , reminder = Orient.ReminderIsSuspended
                            }
                        `shouldBe` Decide.DoNothing

            context "and the details don't match in the update" do
                it "returns a plan to do nothing" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update = Orient.UpdateIsForMultipleJobs
                            , reminder = Orient.ReminderIsSuspended
                            }
                        `shouldBe` Decide.DoNothing

            context "and the message is not an update" do
                it "returns a plan to do nothing" do
                    Decide.decide
                        Orient.Situation
                            { now = toUtc "2021-08-30 15:20:00 PDT"
                            , companyTimeZone = toTimeZone "PDT"
                            , workerId = "10000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , twilioMessageId = "40000000-0000-0000-0000-000000000000"
                            , update = Orient.UpdateIsForMultipleJobs
                            , reminder = Orient.ReminderIsSuspended
                            }
                        `shouldBe` Decide.DoNothing
