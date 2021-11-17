module Tests.Application.Brain.OrientSpec where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Brain.Observe as Observe
import qualified Application.Brain.Orient as Orient
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Twilio.Query as Twilio.Query
import qualified Application.Twilio.View as Twilio.View
import IHP.Prelude
import Test.Hspec
import Tests.Support

spec :: Spec
spec = do
    describe "orient" do
        it "returns a situation based on the given observations" do
            Orient.orient
                Observe.Observations
                    { now = toUtc "2021-09-07 14:00:00 PDT"
                    , companyTimeZone = toTimeZone "PDT"
                    , today = toDay "2021-09-07"
                    , event =
                        Observe.IncomingMessage
                            { message =
                                Twilio.View.Message
                                    { id = "10000000-0000-0000-0000-000000000000"
                                    , fromFirstName = "Bob"
                                    , fromLastName = "Ross"
                                    , fromPhoneNumber = "+14444444444"
                                    , toPhoneNumber = "+15555555555"
                                    , toFirstName = "Michael"
                                    , toLastName = "Phelps"
                                    , createdAt = toUtc "2021-09-07 14:00:00 PDT"
                                    , status = Twilio.Query.Received
                                    , body = "Did some stuff."
                                    , entities =
                                        [ Twilio.View.Entity
                                            { entityType = Twilio.Query.JobName
                                            , rawText = "the good job"
                                            , confidence = 1.0
                                            }
                                        , Twilio.View.Entity
                                            { entityType = Twilio.Query.HoursWorked
                                            , rawText = "7.5 hours"
                                            , confidence = 1.0
                                            }
                                        ]
                                    }
                            , workerId = "20000000-0000-0000-0000-000000000000"
                            , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                            , botPhoneNumberId = "40000000-0000-0000-0000-000000000000"
                            }
                    , timecardEntryRows = []
                    , scheduledReminders =
                        [ SendMessageAction.T
                            { id = "10000000-0000-0000-0000-000000000000"
                            , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                            , state = ActionRunState.notStarted
                            , runsAt = toUtc "2021-09-01 14:00:00 PDT"
                            , body = "Did some good work."
                            , fromId = "30000000-0000-0000-0000-000000000000"
                            , fromNumber = "+15555555555"
                            , toId = "40000000-0000-0000-0000-000000000000"
                            , toNumber = "+16666666666"
                            }
                        ]
                    }
                `shouldBe` Orient.Situation
                    { now = toUtc "2021-09-07 14:00:00 PDT"
                    , companyTimeZone = toTimeZone "PDT"
                    , workerId = "20000000-0000-0000-0000-000000000000"
                    , botPhoneNumberId = "40000000-0000-0000-0000-000000000000"
                    , workerPhoneNumberId = "30000000-0000-0000-0000-000000000000"
                    , twilioMessageId = "10000000-0000-0000-0000-000000000000"
                    , update =
                        Orient.UpdateIsForASingleJob
                            Orient.Job
                                { date = toDay "2021-09-07"
                                , name = "the good job"
                                , clockedInAt = Nothing
                                , clockedOutAt = Nothing
                                , lunchDuration = 30
                                , hoursWorked = 7.5
                                , workDone = "Did some stuff."
                                , invoiceTranslation = "Did some stuff."
                                }
                    , reminder =
                        Orient.ReminderIsScheduled
                            { actionRunStateIds = ["20000000-0000-0000-0000-000000000000"]
                            }
                    }

    describe "buildUpdate" do
        context "UpdateIsForASingleJob" do
            it "returns a single job extracted from a message" do
                Orient.buildUpdate
                    (toDay "2021-09-07")
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-09-06"
                        , accessTokenId = Just "30000000-0000-0000-0000-000000000000"
                        , accessTokenValue = Just "secret"
                        , accessTokenExpiresAt = Just $ toUtc "2021-06-23 15:29:00 PDT"
                        , accessTokenIsRevoked = Just False
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "40000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-09-06"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    ]
                    Twilio.View.Message
                        { id = "50000000-0000-0000-0000-000000000000"
                        , fromFirstName = "Bob"
                        , fromLastName = "Ross"
                        , fromPhoneNumber = "+14444444444"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Michael"
                        , toLastName = "Phelps"
                        , createdAt = toUtc "2021-09-07 14:00:00 PDT"
                        , status = Twilio.Query.Received
                        , body = "Did some stuff."
                        , entities =
                            [ Twilio.View.Entity
                                { entityType = Twilio.Query.JobName
                                , rawText = "the good job"
                                , confidence = 1.0
                                }
                            , Twilio.View.Entity
                                { entityType = Twilio.Query.HoursWorked
                                , rawText = "7.5 hours"
                                , confidence = 1.0
                                }
                            ]
                        }
                    `shouldBe` Orient.UpdateIsForASingleJob
                        Orient.Job
                            { date = toDay "2021-09-07"
                            , name = "the good job"
                            , clockedInAt = Nothing
                            , clockedOutAt = Nothing
                            , lunchDuration = 30
                            , hoursWorked = 7.5
                            , workDone = "Did some stuff."
                            , invoiceTranslation = "Did some stuff."
                            }

            it "uses the job name from a prior timecard entry if no job name was found in the message" do
                Orient.buildUpdate
                    (toDay "2021-09-07")
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-09-06"
                        , accessTokenId = Just "30000000-0000-0000-0000-000000000000"
                        , accessTokenValue = Just "secret"
                        , accessTokenExpiresAt = Just $ toUtc "2021-06-23 15:29:00 PDT"
                        , accessTokenIsRevoked = Just False
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "40000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-09-06"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    ]
                    Twilio.View.Message
                        { id = "50000000-0000-0000-0000-000000000000"
                        , fromFirstName = "Bob"
                        , fromLastName = "Ross"
                        , fromPhoneNumber = "+14444444444"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Michael"
                        , toLastName = "Phelps"
                        , createdAt = toUtc "2021-09-07 14:00:00 PDT"
                        , status = Twilio.Query.Received
                        , body = "Did some stuff."
                        , entities =
                            [ Twilio.View.Entity
                                { entityType = Twilio.Query.HoursWorked
                                , rawText = "7.5 hours"
                                , confidence = 1.0
                                }
                            ]
                        }
                    `shouldBe` Orient.UpdateIsForASingleJob
                        Orient.Job
                            { date = toDay "2021-09-07"
                            , name = "jobName-1"
                            , clockedInAt = Nothing
                            , clockedOutAt = Nothing
                            , lunchDuration = 30
                            , hoursWorked = 7.5
                            , workDone = "Did some stuff."
                            , invoiceTranslation = "Did some stuff."
                            }

        context "UpdateIsForMultipleJobs" do
            it "returns UpdateIsForMultipleJobs if the message had information for multiple jobs" do
                Orient.buildUpdate
                    (toDay "2021-09-07")
                    []
                    Twilio.View.Message
                        { id = "50000000-0000-0000-0000-000000000000"
                        , fromFirstName = "Bob"
                        , fromLastName = "Ross"
                        , fromPhoneNumber = "+14444444444"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Michael"
                        , toLastName = "Phelps"
                        , createdAt = toUtc "2021-09-07 14:00:00 PDT"
                        , status = Twilio.Query.Received
                        , body = "Did some stuff."
                        , entities =
                            [ Twilio.View.Entity
                                { entityType = Twilio.Query.JobName
                                , rawText = "the good job"
                                , confidence = 1.0
                                }
                            , Twilio.View.Entity
                                { entityType = Twilio.Query.HoursWorked
                                , rawText = "7.5 hours"
                                , confidence = 1.0
                                }
                            , Twilio.View.Entity
                                { entityType = Twilio.Query.JobName
                                , rawText = "the bad job"
                                , confidence = 1.0
                                }
                            , Twilio.View.Entity
                                { entityType = Twilio.Query.HoursWorked
                                , rawText = "7.5 hours"
                                , confidence = 1.0
                                }
                            ]
                        }
                    `shouldBe` Orient.UpdateIsForMultipleJobs

        context "UpdateDetailsAreLowConfidence" do
            it "returns UpdateDetailsAreLowConfidence if any of the entities are lower than the confidence threshold" do
                Orient.buildUpdate
                    (toDay "2021-09-07")
                    []
                    Twilio.View.Message
                        { id = "50000000-0000-0000-0000-000000000000"
                        , fromFirstName = "Bob"
                        , fromLastName = "Ross"
                        , fromPhoneNumber = "+14444444444"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Michael"
                        , toLastName = "Phelps"
                        , createdAt = toUtc "2021-09-07 14:00:00 PDT"
                        , status = Twilio.Query.Received
                        , body = "Did some stuff."
                        , entities =
                            [ Twilio.View.Entity
                                { entityType = Twilio.Query.JobName
                                , rawText = "the good job"
                                , confidence = 1.0
                                }
                            , Twilio.View.Entity
                                { entityType = Twilio.Query.ClockedInAt
                                , rawText = "7am"
                                , confidence = 1.0
                                }
                            , Twilio.View.Entity
                                { entityType = Twilio.Query.ClockedOutAt
                                , rawText = "4pm"
                                , confidence = 0.0
                                }
                            , Twilio.View.Entity
                                { entityType = Twilio.Query.HoursWorked
                                , rawText = "7.5 hours"
                                , confidence = 1.0
                                }
                            ]
                        }
                    `shouldBe` Orient.UpdateDetailsAreLowConfidence

        context "UpdateDetailsDoNotMatch" do
            it "returns UpdateDetailsDoNotMatch if the clock information does not match the hours worked" do
                Orient.buildUpdate
                    (toDay "2021-09-07")
                    []
                    Twilio.View.Message
                        { id = "50000000-0000-0000-0000-000000000000"
                        , fromFirstName = "Bob"
                        , fromLastName = "Ross"
                        , fromPhoneNumber = "+14444444444"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Michael"
                        , toLastName = "Phelps"
                        , createdAt = toUtc "2021-09-07 14:00:00 PDT"
                        , status = Twilio.Query.Received
                        , body = "Did some stuff."
                        , entities =
                            [ Twilio.View.Entity
                                { entityType = Twilio.Query.JobName
                                , rawText = "the good job"
                                , confidence = 1.0
                                }
                            , Twilio.View.Entity
                                { entityType = Twilio.Query.ClockedInAt
                                , rawText = "7am"
                                , confidence = 1.0
                                }
                            , Twilio.View.Entity
                                { entityType = Twilio.Query.ClockedOutAt
                                , rawText = "4pm"
                                , confidence = 1.0
                                }
                            , Twilio.View.Entity
                                { entityType = Twilio.Query.HoursWorked
                                , rawText = "7.5 hours"
                                , confidence = 1.0
                                }
                            ]
                        }
                    `shouldBe` Orient.UpdateDetailsDoNotMatch

        context "MessageIsNotAnUpdate" do
            it "returns MessageIsNotAnUpdate if the message is missing clock information and number of hours worked" do
                Orient.buildUpdate
                    (toDay "2021-09-07")
                    [ Timecard.Query.Row
                        { timecardId = "10000000-0000-0000-0000-000000000000"
                        , timecardPersonId = "20000000-0000-0000-0000-000000000000"
                        , timecardWeekOf = toDay "2021-09-06"
                        , accessTokenId = Just "30000000-0000-0000-0000-000000000000"
                        , accessTokenValue = Just "secret"
                        , accessTokenExpiresAt = Just $ toUtc "2021-06-23 15:29:00 PDT"
                        , accessTokenIsRevoked = Just False
                        , signingId = Nothing
                        , signingSignedAt = Nothing
                        , timecardEntryId = "40000000-0000-0000-0000-000000000000"
                        , timecardEntryDate = toDay "2021-09-06"
                        , timecardEntryJobName = "jobName-1"
                        , timecardEntryClockedInAt = Nothing
                        , timecardEntryClockedOutAt = Nothing
                        , timecardEntryLunchDuration = Nothing
                        , timecardEntryHoursWorked = 8.0
                        , timecardEntryWorkDone = "workDone-1"
                        , timecardEntryInvoiceTranslation = "invoiceTranslation-1"
                        }
                    ]
                    Twilio.View.Message
                        { id = "50000000-0000-0000-0000-000000000000"
                        , fromFirstName = "Bob"
                        , fromLastName = "Ross"
                        , fromPhoneNumber = "+14444444444"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Michael"
                        , toLastName = "Phelps"
                        , createdAt = toUtc "2021-09-07 14:00:00 PDT"
                        , status = Twilio.Query.Received
                        , body = "Did some stuff."
                        , entities =
                            [ Twilio.View.Entity
                                { entityType = Twilio.Query.JobName
                                , rawText = "the good job"
                                , confidence = 1.0
                                }
                            ]
                        }
                    `shouldBe` Orient.MessageIsNotAnUpdate

            it "returns MessageIsNotAnUpdate if the message is missing job name and no prior timecard entries exist" do
                Orient.buildUpdate
                    (toDay "2021-09-07")
                    []
                    Twilio.View.Message
                        { id = "50000000-0000-0000-0000-000000000000"
                        , fromFirstName = "Bob"
                        , fromLastName = "Ross"
                        , fromPhoneNumber = "+14444444444"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Michael"
                        , toLastName = "Phelps"
                        , createdAt = toUtc "2021-09-07 14:00:00 PDT"
                        , status = Twilio.Query.Received
                        , body = "Did some stuff."
                        , entities =
                            [ Twilio.View.Entity
                                { entityType = Twilio.Query.HoursWorked
                                , rawText = "7.5 hours"
                                , confidence = 1.0
                                }
                            ]
                        }
                    `shouldBe` Orient.MessageIsNotAnUpdate

    describe "buildReminder" do
        it "returns ReminderIsScheduled when any message action is not suspended" do
            Orient.buildReminder
                [ SendMessageAction.T
                    { id = "10000000-0000-0000-0000-000000000000"
                    , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                    , state = ActionRunState.suspended
                    , runsAt = toUtc "2021-09-01 14:00:00 PDT"
                    , body = "Did some good work."
                    , fromId = "30000000-0000-0000-0000-000000000000"
                    , fromNumber = "+15555555555"
                    , toId = "40000000-0000-0000-0000-000000000000"
                    , toNumber = "+16666666666"
                    }
                , SendMessageAction.T
                    { id = "50000000-0000-0000-0000-000000000000"
                    , actionRunStateId = "60000000-0000-0000-0000-000000000000"
                    , state = ActionRunState.notStarted
                    , runsAt = toUtc "2021-09-01 14:00:00 PDT"
                    , body = "Did some good work."
                    , fromId = "70000000-0000-0000-0000-000000000000"
                    , fromNumber = "+15555555555"
                    , toId = "80000000-0000-0000-0000-000000000000"
                    , toNumber = "+16666666666"
                    }
                ]
                `shouldBe` Orient.ReminderIsScheduled
                    { actionRunStateIds = ["60000000-0000-0000-0000-000000000000"]
                    }

        it "returns ReminderIsSuspended when all send message actions are suspended" do
            Orient.buildReminder
                [ SendMessageAction.T
                    { id = "10000000-0000-0000-0000-000000000000"
                    , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                    , state = ActionRunState.suspended
                    , runsAt = toUtc "2021-09-01 14:00:00 PDT"
                    , body = "Did some good work."
                    , fromId = "30000000-0000-0000-0000-000000000000"
                    , fromNumber = "+15555555555"
                    , toId = "40000000-0000-0000-0000-000000000000"
                    , toNumber = "+16666666666"
                    }
                , SendMessageAction.T
                    { id = "50000000-0000-0000-0000-000000000000"
                    , actionRunStateId = "60000000-0000-0000-0000-000000000000"
                    , state = ActionRunState.suspended
                    , runsAt = toUtc "2021-09-01 14:00:00 PDT"
                    , body = "Did some good work."
                    , fromId = "70000000-0000-0000-0000-000000000000"
                    , fromNumber = "+15555555555"
                    , toId = "80000000-0000-0000-0000-000000000000"
                    , toNumber = "+16666666666"
                    }
                ]
                `shouldBe` Orient.ReminderIsSuspended

        it "returns ReminderIsNotScheduled when the only send message action is canceled" do
            Orient.buildReminder
                [ SendMessageAction.T
                    { id = "10000000-0000-0000-0000-000000000000"
                    , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                    , state = ActionRunState.canceled
                    , runsAt = toUtc "2021-09-01 14:00:00 PDT"
                    , body = "Did some good work."
                    , fromId = "30000000-0000-0000-0000-000000000000"
                    , fromNumber = "+15555555555"
                    , toId = "40000000-0000-0000-0000-000000000000"
                    , toNumber = "+16666666666"
                    }
                ]
                `shouldBe` Orient.ReminderIsNotScheduled

        it "returns ReminderIsNotScheduled when there are no scheduled messages" do
            Orient.buildReminder [] `shouldBe` Orient.ReminderIsNotScheduled

        it "ignores extra send message actions (we are choosing not to handle this case right now)" do
            Orient.buildReminder
                [ SendMessageAction.T
                    { id = "10000000-0000-0000-0000-000000000000"
                    , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                    , state = ActionRunState.notStarted
                    , runsAt = toUtc "2021-09-01 14:00:00 PDT"
                    , body = "Did some good work."
                    , fromId = "30000000-0000-0000-0000-000000000000"
                    , fromNumber = "+15555555555"
                    , toId = "40000000-0000-0000-0000-000000000000"
                    , toNumber = "+16666666666"
                    }
                , SendMessageAction.T
                    { id = "50000000-0000-0000-0000-000000000000"
                    , actionRunStateId = "60000000-0000-0000-0000-000000000000"
                    , state = ActionRunState.suspended
                    , runsAt = toUtc "2021-09-01 14:00:00 PDT"
                    , body = "Did some good work."
                    , fromId = "70000000-0000-0000-0000-000000000000"
                    , fromNumber = "+15555555555"
                    , toId = "80000000-0000-0000-0000-000000000000"
                    , toNumber = "+16666666666"
                    }
                ]
                `shouldBe` Orient.ReminderIsScheduled
                    { actionRunStateIds = ["20000000-0000-0000-0000-000000000000"]
                    }

        it "returns ReminderIsNotScheduled when there are no send message actions" do
            Orient.buildReminder [] `shouldBe` Orient.ReminderIsNotScheduled

    describe "inferHoursWorked" do
        it "returns hours worked if explicitly given" do
            Orient.inferHoursWorked
                (Just $ toTimeOfDay "07:30:00")
                (Just $ toTimeOfDay "10:15:00")
                (Just 8.5)
                `shouldBe` Just 8.5

        it "returns the difference between the clock in and clock out times (minus lunch) if hours worked was not given" do
            Orient.inferHoursWorked
                (Just $ toTimeOfDay "07:30:00")
                (Just $ toTimeOfDay "10:30:00")
                Nothing
                `shouldBe` Just 2.5

        it "returns nothing if we did not have enough information to infer hours worked" do
            Orient.inferHoursWorked
                Nothing
                (Just $ toTimeOfDay "10:30:00")
                Nothing
                `shouldBe` Nothing

    describe "buildJobs" do
        it "returns a list of jobs contained within the given message" do
            Orient.buildJobs
                (toDay "2021-09-01")
                Orient.Message
                    { jobName = ["the good job", "the bad job"]
                    , clockedInAt = [toTimeOfDay "07:00:00"]
                    , clockedOutAt = [toTimeOfDay "16:00:00"]
                    , hoursWorked = [8.5, 7.5]
                    , workDone = "cut some boards"
                    , invoiceTranslation = "trimmed some boards"
                    }
                `shouldBe` [ Orient.Job
                                { date = toDay "2021-09-01"
                                , name = "the good job"
                                , clockedInAt = Just $ toTimeOfDay "07:00:00"
                                , clockedOutAt = Just $ toTimeOfDay "16:00:00"
                                , hoursWorked = 8.5
                                , lunchDuration = 30
                                , workDone = "cut some boards"
                                , invoiceTranslation = "trimmed some boards"
                                }
                           , Orient.Job
                                { date = toDay "2021-09-01"
                                , name = "the bad job"
                                , clockedInAt = Nothing
                                , clockedOutAt = Nothing
                                , hoursWorked = 7.5
                                , lunchDuration = 30
                                , workDone = "cut some boards"
                                , invoiceTranslation = "trimmed some boards"
                                }
                           ]

        it "determines hours worked based on clock information if hours are not given explicitly" do
            Orient.buildJobs
                (toDay "2021-09-01")
                Orient.Message
                    { jobName = ["the good job"]
                    , clockedInAt = [toTimeOfDay "07:00:00"]
                    , clockedOutAt = [toTimeOfDay "16:00:00"]
                    , hoursWorked = []
                    , workDone = "cut some boards"
                    , invoiceTranslation = "trimmed some boards"
                    }
                `shouldBe` [ Orient.Job
                                { date = toDay "2021-09-01"
                                , name = "the good job"
                                , clockedInAt = Just $ toTimeOfDay "07:00:00"
                                , clockedOutAt = Just $ toTimeOfDay "16:00:00"
                                , hoursWorked = 8.5
                                , lunchDuration = 30
                                , workDone = "cut some boards"
                                , invoiceTranslation = "trimmed some boards"
                                }
                           ]

    describe "normalizedMessage" do
        it "includes entities of sufficient confidence" do
            Orient.normalizedMessage
                Nothing
                Twilio.View.Message
                    { id = "10000000-0000-0000-0000-000000000000"
                    , fromFirstName = "Bob"
                    , fromLastName = "Ross"
                    , fromPhoneNumber = "+14444444444"
                    , toPhoneNumber = "+15555555555"
                    , toFirstName = "Michael"
                    , toLastName = "Phelps"
                    , createdAt = toUtc "2021-09-01 14:00:00 PDT"
                    , status = Twilio.Query.Delivered
                    , body = "Did some stuff."
                    , entities =
                        [ Twilio.View.Entity
                            { entityType = Twilio.Query.JobName
                            , rawText = "the good job"
                            , confidence = 0.8
                            }
                        , Twilio.View.Entity
                            { entityType = Twilio.Query.JobName
                            , rawText = "the bad job"
                            , confidence = 0.8
                            }
                        , Twilio.View.Entity
                            { entityType = Twilio.Query.ClockedInAt
                            , rawText = "7:00 am"
                            , confidence = 0.8
                            }
                        , Twilio.View.Entity
                            { entityType = Twilio.Query.ClockedOutAt
                            , rawText = "4:00 pm"
                            , confidence = 0.8
                            }
                        , Twilio.View.Entity
                            { entityType = Twilio.Query.HoursWorked
                            , rawText = "8.5"
                            , confidence = 0.8
                            }
                        , Twilio.View.Entity
                            { entityType = Twilio.Query.HoursWorked
                            , rawText = "7.5"
                            , confidence = 0.8
                            }
                        ]
                    }
                `shouldBe` Orient.Message
                    { jobName = ["the good job", "the bad job"]
                    , clockedInAt = [toTimeOfDay "07:00:00"]
                    , clockedOutAt = [toTimeOfDay "16:00:00"]
                    , hoursWorked = [8.5, 7.5]
                    , workDone = "Did some stuff."
                    , invoiceTranslation = "Did some stuff."
                    }

    describe "normalizedJobNames" do
        it "returns the previous job name if the previous and current job names exist and match" do
            Orient.normalizedJobNames
                (Just "123 Some Job")
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.JobName
                    , rawText = "some job"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` ["123 Some Job"]

        it "returns the current job name if there was no previous job name" do
            Orient.normalizedJobNames
                (Just "123 Something")
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.JobName
                    , rawText = "123 Other Name"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` ["123 Other Name"]

        it "returns the current job name if it does not match the previous job name" do
            Orient.normalizedJobNames
                Nothing
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.JobName
                    , rawText = "some job"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` ["some job"]

        it "returns the previous job name if there are no job name entities" do
            Orient.normalizedJobNames (Just "123 Some Job") [] `shouldBe` ["123 Some Job"]

        it "returns nothing if there was no previous job" do
            Orient.normalizedJobNames
                Nothing
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.ClockedInAt
                    , rawText = "7:30"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` []

        it "returns nothing if there was no previous job name and no job name entities" do
            Orient.normalizedJobNames Nothing [] `shouldBe` []

    describe "normalizedClockedInAts" do
        it "returns the normalized clocked in at time when one is specified as an entity" do
            Orient.normalizedClockedInAts
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.ClockedInAt
                    , rawText = "7:30 am"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [toTimeOfDay "07:30:00"]

        it "returns nothing when no clocked in at entities are specified" do
            Orient.normalizedClockedInAts
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.JobName
                    , rawText = "spiffy"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` []

        it "returns nothing when no entities are specified" do
            Orient.normalizedClockedInAts [] `shouldBe` []

    describe "normalizedClockedOutAts" do
        it "returns the normalized clocked out at time when one is specified as an entity" do
            Orient.normalizedClockedOutAts
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.ClockedOutAt
                    , rawText = "7:30 pm"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [toTimeOfDay "19:30:00"]

        it "returns nothing when no clocked out at entities are specified" do
            Orient.normalizedClockedOutAts
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.ClockedInAt
                    , rawText = "7:30 pm"
                    , confidence = 0.79
                    }
                , Twilio.View.Entity
                    { entityType = Twilio.Query.JobName
                    , rawText = "spiffy"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` []

        it "returns nothing when no entities are specified" do
            Orient.normalizedClockedOutAts [] `shouldBe` []

    describe "normalizedHoursWorked" do
        it "returns the normalized number of hours worked when one is specified as an entity" do
            Orient.normalizedHoursWorked
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.HoursWorked
                    , rawText = "8.5"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [8.5]

    describe "anyLowConfidenceEntities" do
        it "returns true if any of the given entities are low confidence" do
            Orient.anyLowConfidenceEntities
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.HoursWorked
                    , rawText = "8.5"
                    , confidence = 0.4
                    }
                , Twilio.View.Entity
                    { entityType = Twilio.Query.HoursWorked
                    , rawText = "8.5"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` True

        it "returns false if none of the given entities are low confidence" do
            Orient.anyLowConfidenceEntities
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.HoursWorked
                    , rawText = "8.5"
                    , confidence = 0.8
                    }
                , Twilio.View.Entity
                    { entityType = Twilio.Query.HoursWorked
                    , rawText = "8.5"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` False

    describe "hasMultipleJobNames" do
        it "returns true if there are multiple job names" do
            Orient.hasMultipleJobNames
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.JobName
                    , rawText = "Job 1"
                    , confidence = 1.0
                    }
                , Twilio.View.Entity
                    { entityType = Twilio.Query.JobName
                    , rawText = "Job 2"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` True

        it "returns false if there is only one job name" do
            Orient.hasMultipleJobNames
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.JobName
                    , rawText = "Job 1"
                    , confidence = 1.0
                    }
                , Twilio.View.Entity
                    { entityType = Twilio.Query.HoursWorked
                    , rawText = "8 hours"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` False

        it "returns false if there are no job names" do
            Orient.hasMultipleJobNames
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.HoursWorked
                    , rawText = "8 hours"
                    , confidence = 1.0
                    }
                , Twilio.View.Entity
                    { entityType = Twilio.Query.HoursWorked
                    , rawText = "8 hours"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` False

    describe "findEntities" do
        it "returns entities that match the given entity type" do
            Orient.findEntities
                Twilio.Query.WorkDone
                [ Twilio.View.Entity
                    { entityType = Twilio.Query.HoursWorked
                    , rawText = "8.5"
                    , confidence = 1.0
                    }
                , Twilio.View.Entity
                    { entityType = Twilio.Query.WorkDone
                    , rawText = "8.5"
                    , confidence = 1.0
                    }
                ]
                `shouldBe` [ Twilio.View.Entity
                                { entityType = Twilio.Query.WorkDone
                                , rawText = "8.5"
                                , confidence = 1.0
                                }
                           ]

    describe "jobDetailsMatch" do
        it "returns true when clockedInAt and clockedOutAt are not present" do
            Orient.jobDetailsMatch
                Orient.Job
                    { date = toDay "2021-09-01"
                    , name = "the good job"
                    , clockedInAt = Nothing
                    , clockedOutAt = Nothing
                    , hoursWorked = 10.5
                    , lunchDuration = 30
                    , workDone = "cut some boards"
                    , invoiceTranslation = "trimmed some boards"
                    }
                `shouldBe` True

        it "returns true when clockedInAt is not present" do
            Orient.jobDetailsMatch
                Orient.Job
                    { date = toDay "2021-09-01"
                    , name = "the good job"
                    , clockedInAt = Nothing
                    , clockedOutAt = Just $ toTimeOfDay "16:00:00"
                    , hoursWorked = 10.5
                    , lunchDuration = 30
                    , workDone = "cut some boards"
                    , invoiceTranslation = "trimmed some boards"
                    }
                `shouldBe` True

        it "returns true when clockedOutAt is not present" do
            Orient.jobDetailsMatch
                Orient.Job
                    { date = toDay "2021-09-01"
                    , name = "the good job"
                    , clockedInAt = Just $ toTimeOfDay "07:00:00"
                    , clockedOutAt = Nothing
                    , hoursWorked = 10.5
                    , lunchDuration = 30
                    , workDone = "cut some boards"
                    , invoiceTranslation = "trimmed some boards"
                    }
                `shouldBe` True

        context "clockedInAt and clockedOutAt are both present" do
            it "returns true if clock info matches total hours worked within some tolerance" do
                Orient.jobDetailsMatch
                    Orient.Job
                        { date = toDay "2021-09-01"
                        , name = "the good job"
                        , clockedInAt = Just $ toTimeOfDay "07:00:00"
                        , clockedOutAt = Just $ toTimeOfDay "16:00:00"
                        , hoursWorked = 8.6
                        , lunchDuration = 30
                        , workDone = "cut some boards"
                        , invoiceTranslation = "trimmed some boards"
                        }
                    `shouldBe` True

            it "returns false if clock info does not match total hours worked within some tolerance" do
                Orient.jobDetailsMatch
                    Orient.Job
                        { date = toDay "2021-09-01"
                        , name = "the good job"
                        , clockedInAt = Just $ toTimeOfDay "07:00:00"
                        , clockedOutAt = Just $ toTimeOfDay "16:00:00"
                        , hoursWorked = 8.8
                        , lunchDuration = 30
                        , workDone = "cut some boards"
                        , invoiceTranslation = "trimmed some boards"
                        }
                    `shouldBe` False

    describe "entityConfidenceThreshold" do
        it "return 80% as the threshold below which entities will be discarded" do
            Orient.entityConfidenceThreshold `shouldBe` 0.8

    describe "assumedLunchDuration" do
        it "returns 30 minutes" do
            Orient.assumedLunchDuration `shouldBe` 30
