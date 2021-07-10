module Web.View.Communications.IndexSpec where

import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Timecard.View as Timecard.View
import qualified Application.Twilio.Query as Twilio.Query
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support
import Text.Read (read)
import Web.Types
import qualified Web.View.Communications.Index as Index
import Web.View.Navigation.People
import qualified Web.View.Timecards.Status as Status

spec :: Spec
spec = do
    describe "buildPage" do
        it "returns the index page" do
            let barbara =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"
                        |> set #firstName "Barbara"
                        |> set #lastName "Bush"

            let jackie =
                    newRecord @Person
                        |> set #id "20000000-0000-0000-0000-000000000000"
                        |> set #firstName "Jackie"
                        |> set #lastName "Kennedy"

            let people = [barbara, jackie]

            let personSelection = Index.NoPersonSelected

            Index.buildPage Index.IndexView {..}
                `shouldBe` Index.Page
                    { selectedPerson = Nothing
                    , peopleNavigation =
                        PeopleNavigation
                            [ PersonItem
                                { selectionAction =
                                    CommunicationsPersonSelectionAction
                                        { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                        }
                                , activeClass = ""
                                , ariaCurrent = "false"
                                , firstName = "Barbara"
                                , lastName = "Bush"
                                }
                            , PersonItem
                                { selectionAction =
                                    CommunicationsPersonSelectionAction
                                        { selectedPersonId = "20000000-0000-0000-0000-000000000000"
                                        }
                                , activeClass = ""
                                , ariaCurrent = "false"
                                , firstName = "Jackie"
                                , lastName = "Kennedy"
                                }
                            ]
                    , messagesColumn = Index.MessagesColumnNotVisible
                    , timecardColumn = Index.TimecardList []
                    }

    describe "buildMessagesColumn" do
        it "returns a visible messages column when a person is selected" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let people = [person]

            let phoneNumber =
                    newRecord @PhoneNumber
                        |> set #id "20000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "30000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity = Index.SendingMessage {timecards = []}

            let twilioMessage1 =
                    Twilio.Query.Row
                        { id = "40000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }

            let twilioMessage2 =
                    Twilio.Query.Row
                        { id = "50000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+16666666666"
                        , fromFirstName = "Jackie"
                        , fromLastName = "Kennedy"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Barbara"
                        , toLastName = "Bush"
                        , createdAt = toUtc "2021-06-23 15:30:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "Not much."
                        }

            let scheduledMessage =
                    SendMessageAction.T
                        { id = "60000000-0000-0000-0000-000000000000"
                        , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                        , state = "not_running"
                        , runsAt = toUtc "2021-06-23 15:30:00 PDT"
                        , body = "Hello World!"
                        , fromId = "30000000-0000-0000-0000-000000000000"
                        , fromNumber = "+15555555555"
                        , toId = "40000000-0000-0000-0000-000000000000"
                        , toNumber = "+16666666666"
                        }

            let personSelection =
                    Index.PersonSelected
                        { selectedPerson = person
                        , messages = [twilioMessage1, twilioMessage2]
                        , toPhoneNumber = phoneNumber
                        , scheduledMessages = [scheduledMessage]
                        , newMessage = newRecord @TwilioMessage
                        , personActivity = personActivity
                        }

            Index.buildMessagesColumn Index.IndexView {..}
                `shouldBe` Index.MessagesColumnVisible
                    { messageItems =
                        [ Index.MessageItem
                            { fromName = "Barbara Bush"
                            , sentAt = "2021-06-23 22:29:00 UTC"
                            , body = "What's up?"
                            , statusClass = "message-status delivered"
                            , status = "Delivered"
                            , linkButtonActiveClass = ""
                            , linkButtonText = "Link"
                            , linkButtonAction =
                                CommunicationsNewTimecardEntryAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    , selectedMessageIds = ["40000000-0000-0000-0000-000000000000"]
                                    }
                            }
                        , Index.MessageItem
                            { fromName = "Jackie Kennedy"
                            , sentAt = "2021-06-23 22:30:00 UTC"
                            , body = "Not much."
                            , statusClass = "message-status delivered"
                            , status = "Delivered"
                            , linkButtonActiveClass = ""
                            , linkButtonText = "Link"
                            , linkButtonAction =
                                CommunicationsNewTimecardEntryAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    , selectedMessageIds = ["50000000-0000-0000-0000-000000000000"]
                                    }
                            }
                        ]
                    , scheduledMessageItems =
                        [ Index.ScheduledMessageItem
                            { runsAt = "2021-06-23 22:30:00 UTC"
                            , body = "Hello World!"
                            , cancelAction =
                                CommunicationsCancelScheduledMessageAction
                                    { sendMessageActionId = "60000000-0000-0000-0000-000000000000"
                                    }
                            }
                        ]
                    , sendMessageForm =
                        Index.SendMessageForm
                            { toPhoneNumberId = "20000000-0000-0000-0000-000000000000"
                            }
                    }

        it "returns a non-visible messages column when no person is selected" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let people = [person]

            let personSelection = Index.NoPersonSelected

            Index.buildMessagesColumn Index.IndexView {..}
                `shouldBe` Index.MessagesColumnNotVisible

    describe "buildMessageItems" do
        it "returns a list of messages that are currently linked when we are editing a timecard entry" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity = Index.SendingMessage {timecards = []}

            let twilioMessage1 =
                    Twilio.Query.Row
                        { id = "30000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }

            let twilioMessage2 =
                    Twilio.Query.Row
                        { id = "40000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+16666666666"
                        , fromFirstName = "Jackie"
                        , fromLastName = "Kennedy"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Barbara"
                        , toLastName = "Bush"
                        , createdAt = toUtc "2021-06-23 15:30:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "Not much."
                        }

            Index.buildMessageItems
                person
                personActivity
                [twilioMessage1, twilioMessage2]
                `shouldBe` [ Index.MessageItem
                                { fromName = "Barbara Bush"
                                , sentAt = "2021-06-23 22:29:00 UTC"
                                , body = "What's up?"
                                , statusClass = "message-status delivered"
                                , status = "Delivered"
                                , linkButtonActiveClass = ""
                                , linkButtonText = "Link"
                                , linkButtonAction =
                                    CommunicationsNewTimecardEntryAction
                                        { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                        , selectedMessageIds =
                                            [ "30000000-0000-0000-0000-000000000000"
                                            ]
                                        }
                                }
                           , Index.MessageItem
                                { fromName = "Jackie Kennedy"
                                , sentAt = "2021-06-23 22:30:00 UTC"
                                , body = "Not much."
                                , statusClass = "message-status delivered"
                                , status = "Delivered"
                                , linkButtonActiveClass = ""
                                , linkButtonText = "Link"
                                , linkButtonAction =
                                    CommunicationsNewTimecardEntryAction
                                        { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                        , selectedMessageIds = ["40000000-0000-0000-0000-000000000000"]
                                        }
                                }
                           ]

        it "returns a list of unlinked message items when we are in sending message mode" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #id "20000000-0000-0000-0000-000000000000"

            let twilioMessage1 =
                    Twilio.Query.Row
                        { id = "30000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }

            let twilioMessage2 =
                    Twilio.Query.Row
                        { id = "40000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+16666666666"
                        , fromFirstName = "Jackie"
                        , fromLastName = "Kennedy"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Barbara"
                        , toLastName = "Bush"
                        , createdAt = toUtc "2021-06-23 15:30:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "Not much."
                        }

            let personActivity =
                    Index.WorkingOnTimecardEntry
                        { timecardEntry = timecardEntry
                        , selectedMessages = [twilioMessage2]
                        , timecardActivity = Index.CreatingEntry
                        }

            Index.buildMessageItems
                person
                personActivity
                [twilioMessage1, twilioMessage2]
                `shouldBe` [ Index.MessageItem
                                { fromName = "Barbara Bush"
                                , sentAt = "2021-06-23 22:29:00 UTC"
                                , body = "What's up?"
                                , statusClass = "message-status delivered"
                                , status = "Delivered"
                                , linkButtonActiveClass = ""
                                , linkButtonText = "Link"
                                , linkButtonAction =
                                    CommunicationsNewTimecardEntryAction
                                        { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                        , selectedMessageIds = ["30000000-0000-0000-0000-000000000000", "40000000-0000-0000-0000-000000000000"]
                                        }
                                }
                           , Index.MessageItem
                                { fromName = "Jackie Kennedy"
                                , sentAt = "2021-06-23 22:30:00 UTC"
                                , body = "Not much."
                                , statusClass = "message-status delivered"
                                , status = "Delivered"
                                , linkButtonActiveClass = "active"
                                , linkButtonText = "Unlink"
                                , linkButtonAction =
                                    CommunicationsNewTimecardEntryAction
                                        { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                        , selectedMessageIds = []
                                        }
                                }
                           ]

    describe "buildMessageItem" do
        it "returns a message item with a link to create a new timecard entry when we are in sending message mode" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #id "20000000-0000-0000-0000-000000000000"

            let twilioMessage =
                    Twilio.Query.Row
                        { id = "30000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }

            let personActivity = Index.SendingMessage {timecards = []}

            Index.buildMessageItem
                person
                personActivity
                [ "30000000-0000-0000-0000-000000000000"
                ]
                twilioMessage
                `shouldBe` Index.MessageItem
                    { fromName = "Barbara Bush"
                    , sentAt = "2021-06-23 22:29:00 UTC"
                    , body = "What's up?"
                    , statusClass = "message-status delivered"
                    , status = "Delivered"
                    , linkButtonActiveClass = "active"
                    , linkButtonText = "Unlink"
                    , linkButtonAction =
                        CommunicationsNewTimecardEntryAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , selectedMessageIds = []
                            }
                    }

        it "returns a message item with a link to create a new timecard entry when we are editing a timecard entry" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #id "20000000-0000-0000-0000-000000000000"

            let twilioMessage =
                    Twilio.Query.Row
                        { id = "30000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }

            let personActivity =
                    Index.WorkingOnTimecardEntry
                        { timecardEntry = timecardEntry
                        , selectedMessages = [twilioMessage]
                        , timecardActivity = Index.CreatingEntry
                        }

            Index.buildMessageItem
                person
                personActivity
                [ "40000000-0000-0000-0000-000000000000"
                ]
                twilioMessage
                `shouldBe` Index.MessageItem
                    { fromName = "Barbara Bush"
                    , sentAt = "2021-06-23 22:29:00 UTC"
                    , body = "What's up?"
                    , statusClass = "message-status delivered"
                    , status = "Delivered"
                    , linkButtonActiveClass = ""
                    , linkButtonText = "Link"
                    , linkButtonAction =
                        CommunicationsNewTimecardEntryAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , selectedMessageIds =
                                [ "30000000-0000-0000-0000-000000000000"
                                , "40000000-0000-0000-0000-000000000000"
                                ]
                            }
                    }

        it "returns a message to unlink the message if it is currently linked" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #id "20000000-0000-0000-0000-000000000000"

            let twilioMessage =
                    Twilio.Query.Row
                        { id = "30000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }

            let personActivity =
                    Index.WorkingOnTimecardEntry
                        { timecardEntry = timecardEntry
                        , selectedMessages = [twilioMessage]
                        , timecardActivity = Index.CreatingEntry
                        }

            Index.buildMessageItem
                person
                personActivity
                [ "30000000-0000-0000-0000-000000000000"
                , "40000000-0000-0000-0000-000000000000"
                ]
                twilioMessage
                `shouldBe` Index.MessageItem
                    { fromName = "Barbara Bush"
                    , sentAt = "2021-06-23 22:29:00 UTC"
                    , body = "What's up?"
                    , statusClass = "message-status delivered"
                    , status = "Delivered"
                    , linkButtonActiveClass = "active"
                    , linkButtonText = "Unlink"
                    , linkButtonAction =
                        CommunicationsNewTimecardEntryAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , selectedMessageIds = ["40000000-0000-0000-0000-000000000000"]
                            }
                    }

    describe "messageStatusClass" do
        it "returns a delivered message status class when the message has been delivered" do
            Index.messageStatusClass Twilio.Query.Delivered
                `shouldBe` "message-status delivered"

        it "returns a received message status class when the message has been received" do
            Index.messageStatusClass Twilio.Query.Received
                `shouldBe` "message-status received"

        it "returns a failed message status class when the message has failed to send" do
            Index.messageStatusClass Twilio.Query.Failed
                `shouldBe` "message-status failed"

        it "returns a sending message status class for other statuses" do
            Index.messageStatusClass Twilio.Query.Queued
                `shouldBe` "message-status sending"

    describe "buildScheduledMessageItem" do
        it "returns a scheduled message item based on the given parameters" do
            let sendMessageAction =
                    SendMessageAction.T
                        { id = "10000000-0000-0000-0000-000000000000"
                        , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                        , state = "not_running"
                        , runsAt = toUtc "2021-06-23 15:30:00 PDT"
                        , body = "Hello World!"
                        , fromId = "30000000-0000-0000-0000-000000000000"
                        , fromNumber = "+15555555555"
                        , toId = "40000000-0000-0000-0000-000000000000"
                        , toNumber = "+16666666666"
                        }

            Index.buildScheduledMessageItem sendMessageAction
                `shouldBe` Index.ScheduledMessageItem
                    { runsAt = "2021-06-23 22:30:00 UTC"
                    , body = "Hello World!"
                    , cancelAction =
                        CommunicationsCancelScheduledMessageAction
                            "10000000-0000-0000-0000-000000000000"
                    }

    describe "buildSendMessageForm" do
        it "returns a send message form based on the given parameters" do
            let phoneNumber =
                    newRecord @PhoneNumber
                        |> set #id "10000000-0000-0000-0000-000000000000"

            Index.buildSendMessageForm phoneNumber
                `shouldBe` Index.SendMessageForm
                    { toPhoneNumberId = "10000000-0000-0000-0000-000000000000"
                    }

    describe "buildTimecardColumn" do
        it "returns a timecard column with timecard blocks when we are in message sending mode" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let people = [person]

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecard =
                    Timecard.View.Timecard
                        { id = "30000000-0000-0000-0000-000000000000"
                        , personId = "40000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-06-21"
                        , status = Timecard.View.TimecardInProgress
                        , entries = [timecardEntry]
                        }

            let personSelection =
                    Index.PersonSelected
                        { selectedPerson = person
                        , messages = []
                        , toPhoneNumber = newRecord @PhoneNumber
                        , scheduledMessages = []
                        , newMessage = newRecord @TwilioMessage
                        , personActivity =
                            Index.SendingMessage
                                { timecards = [timecard]
                                }
                        }
            Index.buildTimecardColumn Index.IndexView {..}
                `shouldBe` Index.TimecardList
                    [ Index.TimecardBlock
                        { weekOf = "06/21/2021"
                        , status =
                            Status.TimecardStatus
                                { statusClasses = "badge badge-pill badge-secondary"
                                , statusLabel = "In Progress"
                                }
                        , actions = Index.TimecardInProgress
                        , entryCards =
                            [ Index.TimecardEntryCard
                                { dayOfWeek' = "Wednesday"
                                , date = "06/23/2021"
                                , jobName = "job name"
                                , invoiceTranslation = "invoice translation"
                                , editAction =
                                    CommunicationsEditTimecardEntryAction
                                        { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                        , timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                        }
                                }
                            ]
                        }
                    ]

        it "returns a timecard column with a timecard entry form when we are linking messages" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let people = [person]

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #id "20000000-0000-0000-0000-000000000000"
                        |> set #date (toDay "2021-06-21")

            let twilioMessage =
                    Twilio.Query.Row
                        { id = "30000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }

            let personSelection =
                    Index.PersonSelected
                        { selectedPerson = person
                        , messages = []
                        , toPhoneNumber = newRecord @PhoneNumber
                        , scheduledMessages = []
                        , newMessage = newRecord @TwilioMessage
                        , personActivity =
                            Index.WorkingOnTimecardEntry
                                { timecardEntry = timecardEntry
                                , selectedMessages = [twilioMessage]
                                , timecardActivity = Index.CreatingEntry
                                }
                        }
            Index.buildTimecardColumn Index.IndexView {..}
                `shouldBe` Index.EditTimecardEntry
                    { timecardEntryForm =
                        Index.TimecardEntryForm
                            { date = "2021-06-21"
                            , dateInvalidClass = ""
                            , dateError = Nothing
                            , jobName = ""
                            , jobNameInvalidClass = ""
                            , jobNameError = Nothing
                            , hoursWorked = "0.0"
                            , hoursWorkedInvalidClass = ""
                            , hoursWorkedError = Nothing
                            , workDone = "What's up?"
                            , workDoneInvalidClass = ""
                            , workDoneError = Nothing
                            , invoiceTranslation = "What's up?"
                            , invoiceTranslationInvalidClass = ""
                            , invoiceTranslationError = Nothing
                            , selectedMessageIdsParam = "30000000-0000-0000-0000-000000000000"
                            , selectedPersonIdParam = "10000000-0000-0000-0000-000000000000"
                            , submitLabel = "Create"
                            , submitAction = CommunicationsCreateTimecardEntryAction
                            , cancelAction =
                                CommunicationsPersonSelectionAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    }
                            }
                    }

    describe "buildTimecardBlock" do
        it "returns a timecard block based on the given parameters" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecard =
                    Timecard.View.Timecard
                        { id = "30000000-0000-0000-0000-000000000000"
                        , personId = "40000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-06-21"
                        , status = Timecard.View.TimecardInProgress
                        , entries = [timecardEntry]
                        }

            Index.buildTimecardBlock person timecard
                `shouldBe` Index.TimecardBlock
                    { weekOf = "06/21/2021"
                    , status = Status.TimecardStatus {statusClasses = "badge badge-pill badge-secondary", statusLabel = "In Progress"}
                    , actions = Index.TimecardInProgress
                    , entryCards =
                        [ Index.TimecardEntryCard
                            { dayOfWeek' = "Wednesday"
                            , date = "06/23/2021"
                            , jobName = "job name"
                            , invoiceTranslation = "invoice translation"
                            , editAction =
                                CommunicationsEditTimecardEntryAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    , timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                    }
                            }
                        ]
                    }

    describe "buildTimecardActions" do
        it "returns in progress when the timecard is in progress" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecard =
                    Timecard.View.Timecard
                        { id = "30000000-0000-0000-0000-000000000000"
                        , personId = "40000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-06-21"
                        , status = Timecard.View.TimecardInProgress
                        , entries = [timecardEntry]
                        }

            Index.buildTimecardActions person timecard
                `shouldBe` Index.TimecardInProgress

        it "returns ready for review when the timecard is ready for review" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecard =
                    Timecard.View.Timecard
                        { id = "30000000-0000-0000-0000-000000000000"
                        , personId = "40000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-06-21"
                        , status = Timecard.View.TimecardReadyForReview
                        , entries = [timecardEntry]
                        }

            Index.buildTimecardActions person timecard
                `shouldBe` Index.TimecardReadyForReview
                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                    , timecardId = "30000000-0000-0000-0000-000000000000"
                    }

        it "returns under review when the timecard is under review" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let accessToken =
                    Timecard.View.AccessToken
                        { id = "30000000-0000-0000-0000-000000000000"
                        , value = "secret"
                        , expiresAt = toUtc "2021-06-23 15:30:00 PDT"
                        , isRevoked = False
                        }

            let timecard =
                    Timecard.View.Timecard
                        { id = "40000000-0000-0000-0000-000000000000"
                        , personId = "50000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-06-21"
                        , status = Timecard.View.TimecardUnderReview accessToken
                        , entries = [timecardEntry]
                        }

            Index.buildTimecardActions person timecard
                `shouldBe` Index.TimecardUnderReview
                    { reviewAction = ShowTimecardReviewAction "secret"
                    }

        it "returns signed when the timecard is signed" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let signing =
                    Timecard.View.Signing
                        { id = "30000000-0000-0000-0000-000000000000"
                        , signedAt = toUtc "2021-06-23 15:30:00 PDT"
                        }

            let timecard =
                    Timecard.View.Timecard
                        { id = "40000000-0000-0000-0000-000000000000"
                        , personId = "50000000-0000-0000-0000-000000000000"
                        , weekOf = toDay "2021-06-21"
                        , status = Timecard.View.TimecardSigned signing
                        , entries = [timecardEntry]
                        }

            Index.buildTimecardActions person timecard
                `shouldBe` Index.TimecardSigned

    describe "buildTimecardEntryCard" do
        it "returns a timecard entry card based on the given parameters" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let timecardEntryCard =
                    Index.buildTimecardEntryCard
                        person
                        timecardEntry

            timecardEntryCard
                `shouldBe` Index.TimecardEntryCard
                    { dayOfWeek' = "Wednesday"
                    , date = "06/23/2021"
                    , jobName = "job name"
                    , invoiceTranslation = "invoice translation"
                    , editAction =
                        CommunicationsEditTimecardEntryAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , timecardEntryId = "20000000-0000-0000-0000-000000000000"
                            }
                    }

    describe "buildTimecardEntryForm" do
        it "sets error fields to nothing when no errors are present" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let twilioMessage =
                    Twilio.Query.Row
                        { id = "20000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }

            let timecardActivity = Index.CreatingEntry

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #date (toDay "2021-06-23")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"

            let timecardEntryForm =
                    Index.buildTimecardEntryForm
                        person
                        [twilioMessage]
                        timecardActivity
                        timecardEntry

            timecardEntryForm
                `shouldBe` Index.TimecardEntryForm
                    { date = "2021-06-23"
                    , dateInvalidClass = ""
                    , dateError = Nothing
                    , jobName = "McDonald's"
                    , jobNameInvalidClass = ""
                    , jobNameError = Nothing
                    , hoursWorked = "8.0"
                    , hoursWorkedInvalidClass = ""
                    , hoursWorkedError = Nothing
                    , workDone = "work"
                    , workDoneInvalidClass = ""
                    , workDoneError = Nothing
                    , invoiceTranslation = "invoice"
                    , invoiceTranslationInvalidClass = ""
                    , invoiceTranslationError = Nothing
                    , selectedMessageIdsParam = "20000000-0000-0000-0000-000000000000"
                    , selectedPersonIdParam = "10000000-0000-0000-0000-000000000000"
                    , submitLabel = "Create"
                    , submitAction = CommunicationsCreateTimecardEntryAction
                    , cancelAction =
                        CommunicationsPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            }
                    }

        it "sets error fields when errors are present" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let twilioMessage =
                    Twilio.Query.Row
                        { id = "20000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }

            let timecardActivity = Index.CreatingEntry

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #date (toDay "2021-06-23")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"
                        |> validateField #date (\_ -> Failure "date error")
                        |> validateField #jobName (\_ -> Failure "jobName error")
                        |> validateField #hoursWorked (\_ -> Failure "hoursWorked error")
                        |> validateField #workDone (\_ -> Failure "workDone error")
                        |> validateField #invoiceTranslation (\_ -> Failure "invoiceTranslation error")

            let timecardEntryForm =
                    Index.buildTimecardEntryForm
                        person
                        [twilioMessage]
                        timecardActivity
                        timecardEntry

            timecardEntryForm
                `shouldBe` Index.TimecardEntryForm
                    { date = "2021-06-23"
                    , dateInvalidClass = "is-invalid"
                    , dateError = Just "date error"
                    , jobName = "McDonald's"
                    , jobNameInvalidClass = "is-invalid"
                    , jobNameError = Just "jobName error"
                    , hoursWorked = "8.0"
                    , hoursWorkedInvalidClass = "is-invalid"
                    , hoursWorkedError = Just "hoursWorked error"
                    , workDone = "work"
                    , workDoneInvalidClass = "is-invalid"
                    , workDoneError = Just "workDone error"
                    , invoiceTranslation = "invoice"
                    , invoiceTranslationInvalidClass = "is-invalid"
                    , invoiceTranslationError = Just "invoiceTranslation error"
                    , selectedMessageIdsParam = "20000000-0000-0000-0000-000000000000"
                    , selectedPersonIdParam = "10000000-0000-0000-0000-000000000000"
                    , submitLabel = "Create"
                    , submitAction = CommunicationsCreateTimecardEntryAction
                    , cancelAction =
                        CommunicationsPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            }
                    }

        it "sets submit label and action to update when the timecard activity is editing" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let twilioMessage =
                    Twilio.Query.Row
                        { id = "20000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }

            let timecardActivity = Index.EditingEntry

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #id "30000000-0000-0000-0000-000000000000"
                        |> set #date (toDay "2021-06-23")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone "work"
                        |> set #invoiceTranslation "invoice"

            let timecardEntryForm =
                    Index.buildTimecardEntryForm
                        person
                        [twilioMessage]
                        timecardActivity
                        timecardEntry

            timecardEntryForm
                `shouldBe` Index.TimecardEntryForm
                    { date = "2021-06-23"
                    , dateInvalidClass = ""
                    , dateError = Nothing
                    , jobName = "McDonald's"
                    , jobNameInvalidClass = ""
                    , jobNameError = Nothing
                    , hoursWorked = "8.0"
                    , hoursWorkedInvalidClass = ""
                    , hoursWorkedError = Nothing
                    , workDone = "work"
                    , workDoneInvalidClass = ""
                    , workDoneError = Nothing
                    , invoiceTranslation = "invoice"
                    , invoiceTranslationInvalidClass = ""
                    , invoiceTranslationError = Nothing
                    , selectedMessageIdsParam = "20000000-0000-0000-0000-000000000000"
                    , selectedPersonIdParam = "10000000-0000-0000-0000-000000000000"
                    , submitLabel = "Update"
                    , submitAction =
                        CommunicationsUpdateTimecardEntryAction
                            { timecardEntryId = "30000000-0000-0000-0000-000000000000"
                            }
                    , cancelAction =
                        CommunicationsPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            }
                    }

        it "concatenates message bodies in order of creation time" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let twilioMessage1 =
                    Twilio.Query.Row
                        { id = "20000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }
            let twilioMessage2 =
                    Twilio.Query.Row
                        { id = "30000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+16666666666"
                        , fromFirstName = "Jackie"
                        , fromLastName = "Kennedy"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Barbara"
                        , toLastName = "Bush"
                        , createdAt = toUtc "2021-06-23 15:30:00 PDT"
                        , status = Twilio.Query.Received
                        , body = "Nothing much."
                        }

            let timecardActivity = Index.CreatingEntry

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #date (toDay "2021-06-23")
                        |> set #jobName "McDonald's"
                        |> set #hoursWorked 8.0
                        |> set #workDone ""
                        |> set #invoiceTranslation ""

            let timecardEntryForm =
                    Index.buildTimecardEntryForm
                        person
                        [twilioMessage2, twilioMessage1]
                        timecardActivity
                        timecardEntry

            timecardEntryForm
                `shouldBe` Index.TimecardEntryForm
                    { date = "2021-06-23"
                    , dateInvalidClass = ""
                    , dateError = Nothing
                    , jobName = "McDonald's"
                    , jobNameInvalidClass = ""
                    , jobNameError = Nothing
                    , hoursWorked = "8.0"
                    , hoursWorkedInvalidClass = ""
                    , hoursWorkedError = Nothing
                    , workDone = "What's up?\n\nNothing much."
                    , workDoneInvalidClass = ""
                    , workDoneError = Nothing
                    , invoiceTranslation = "What's up?\n\nNothing much."
                    , invoiceTranslationInvalidClass = ""
                    , invoiceTranslationError = Nothing
                    , selectedMessageIdsParam = "20000000-0000-0000-0000-000000000000,30000000-0000-0000-0000-000000000000"
                    , selectedPersonIdParam = "10000000-0000-0000-0000-000000000000"
                    , submitLabel = "Create"
                    , submitAction = CommunicationsCreateTimecardEntryAction
                    , cancelAction = CommunicationsPersonSelectionAction {selectedPersonId = "10000000-0000-0000-0000-000000000000"}
                    }

    describe "assembleMessageBodies" do
        it "returns the concatenated bodies of all of the twilio messages if the existing text is blank" do
            let twilioMessage1 =
                    Twilio.Query.Row
                        { id = "10000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }
            let twilioMessage2 =
                    Twilio.Query.Row
                        { id = "20000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+16666666666"
                        , fromFirstName = "Jackie"
                        , fromLastName = "Kennedy"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Barbara"
                        , toLastName = "Bush"
                        , createdAt = toUtc "2021-06-23 15:30:00 PDT"
                        , status = Twilio.Query.Received
                        , body = "Nothing much."
                        }
            Index.assembleMessageBodies "" [twilioMessage1, twilioMessage2]
                `shouldBe` "What's up?\n\nNothing much."

        it "returns the existing text and ignores the messages if the existing text is not blank" do
            let twilioMessage1 =
                    Twilio.Query.Row
                        { id = "10000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+15555555555"
                        , fromFirstName = "Barbara"
                        , fromLastName = "Bush"
                        , toPhoneNumber = "+16666666666"
                        , toFirstName = "Jackie"
                        , toLastName = "Kennedy"
                        , createdAt = toUtc "2021-06-23 15:29:00 PDT"
                        , status = Twilio.Query.Delivered
                        , body = "What's up?"
                        }
            let twilioMessage2 =
                    Twilio.Query.Row
                        { id = "20000000-0000-0000-0000-000000000000"
                        , fromPhoneNumber = "+16666666666"
                        , fromFirstName = "Jackie"
                        , fromLastName = "Kennedy"
                        , toPhoneNumber = "+15555555555"
                        , toFirstName = "Barbara"
                        , toLastName = "Bush"
                        , createdAt = toUtc "2021-06-23 15:30:00 PDT"
                        , status = Twilio.Query.Received
                        , body = "Nothing much."
                        }
            Index.assembleMessageBodies "Existing text" [twilioMessage1, twilioMessage2]
                `shouldBe` "Existing text"
