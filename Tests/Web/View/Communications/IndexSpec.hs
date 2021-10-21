module Tests.Web.View.Communications.IndexSpec where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.People.View as People.View
import qualified Application.Timecard.View as Timecard.View
import qualified Application.Twilio.Query as Twilio.Query
import qualified Application.Twilio.View as Twilio.View
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Tests.Support
import Web.Types
import qualified Web.View.Communications.Index as Index
import Web.View.Navigation.People
import qualified Web.View.Timecards.Status as Status

spec :: Spec
spec = do
    describe "buildPage" do
        it "returns the index page" do
            let barbara =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            let jackie =
                    People.View.Person
                        { id = "20000000-0000-0000-0000-000000000000"
                        , firstName = "Jackie"
                        , lastName = "Kennedy"
                        , goesBy = "Jackie"
                        , state = People.View.PersonIdle
                        }

            let people = [barbara, jackie]

            let personSelection = Index.NoPersonSelected

            let currentColumn = Index.PeopleColumn

            Index.buildPage Index.IndexView {..}
                `shouldBe` Index.Page
                    { selectedPerson = Nothing
                    , peopleNavigationClasses = "d-flex flex-grow-1 flex-lg-grow-0"
                    , peopleNavigation =
                        PeopleNavigation
                            [ PersonItem
                                { selectionAction =
                                    CommunicationsPersonSelectionAction
                                        { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                        , column = Just "messages"
                                        }
                                , activeClass = ""
                                , ariaCurrent = "false"
                                , firstName = "Barbara"
                                , lastName = "Bush"
                                , stateBadge =
                                    VisibleBadge
                                        { label = "Idle"
                                        , classes = "badge badge-pill badge-light"
                                        }
                                }
                            , PersonItem
                                { selectionAction =
                                    CommunicationsPersonSelectionAction
                                        { selectedPersonId = "20000000-0000-0000-0000-000000000000"
                                        , column = Just "messages"
                                        }
                                , activeClass = ""
                                , ariaCurrent = "false"
                                , firstName = "Jackie"
                                , lastName = "Kennedy"
                                , stateBadge =
                                    VisibleBadge
                                        { label = "Idle"
                                        , classes = "badge badge-pill badge-light"
                                        }
                                }
                            ]
                    , messagesColumnClasses = "d-none d-lg-flex"
                    , messagesColumn = Index.MessagesColumnNotVisible
                    , timecardsColumnClasses = "d-none d-lg-flex"
                    , timecardsColumn = Index.TimecardList []
                    , columnNavigation =
                        Index.ColumnNavigation
                            { peopleLinkClass = "text-dark"
                            , peopleAction = CommunicationsAction
                            , messagesLinkClass = "text-muted"
                            , messagesAction = CommunicationsAction
                            , timecardsLinkClass = "text-muted"
                            , timecardsAction = CommunicationsAction
                            }
                    }

    describe "columnClasses" do
        context "when the given column is in view" do
            it "returns classes that allow the column to expand to the whole width of the screen on mobile" do
                Index.columnClasses Index.PeopleColumn Index.PeopleColumn
                    `shouldBe` "d-flex flex-grow-1 flex-lg-grow-0"
        context "when the given column is not in view" do
            it "returns classes that hide the column on mobile" do
                Index.columnClasses Index.PeopleColumn Index.TimecardsColumn
                    `shouldBe` "d-none d-lg-flex"

    describe "buildMessagesColumn" do
        it "returns a visible messages column when a person is selected" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            let people = [person]

            let selectedPerson =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let phoneNumber =
                    newRecord @PhoneNumber
                        |> set #id "20000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "30000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity = Index.SendingMessage {timecards = []}

            let twilioMessage1 =
                    Twilio.View.Message
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
                        , entities = []
                        }

            let twilioMessage2 =
                    Twilio.View.Message
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
                        , entities = []
                        }

            let scheduledMessage =
                    SendMessageAction.T
                        { id = "60000000-0000-0000-0000-000000000000"
                        , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                        , state = ActionRunState.notStarted
                        , runsAt = toUtc "2021-06-23 15:30:00 PDT"
                        , body = "Hello World!"
                        , fromId = "30000000-0000-0000-0000-000000000000"
                        , fromNumber = "+15555555555"
                        , toId = "40000000-0000-0000-0000-000000000000"
                        , toNumber = "+16666666666"
                        }

            let personSelection =
                    Index.PersonSelected
                        { selectedPerson = selectedPerson
                        , messages = [twilioMessage1, twilioMessage2]
                        , toPhoneNumber = phoneNumber
                        , scheduledMessages = [scheduledMessage]
                        , editingScheduledMessageId = Nothing
                        , newMessage = newRecord @TwilioMessage
                        , personActivity = personActivity
                        }

            let currentColumn = Index.TimecardsColumn

            Index.buildMessagesColumn Index.IndexView {..}
                `shouldBe` Index.MessagesColumnVisible
                    { messageItems =
                        [ Index.MessageItem
                            { fromName = "Barbara Bush"
                            , sentAt = "2021-06-23T22:29:00+0000"
                            , entities = []
                            , statusClass = "message-status delivered"
                            , status = "Delivered"
                            , linkButtonActiveClass = ""
                            , linkButtonText = "Link"
                            , linkButtonAction =
                                CommunicationsNewTimecardEntryAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    , selectedMessageIds =
                                        [ "40000000-0000-0000-0000-000000000000"
                                        ]
                                    }
                            }
                        , Index.MessageItem
                            { fromName = "Jackie Kennedy"
                            , sentAt = "2021-06-23T22:30:00+0000"
                            , entities = []
                            , statusClass = "message-status delivered"
                            , status = "Delivered"
                            , linkButtonActiveClass = ""
                            , linkButtonText = "Link"
                            , linkButtonAction =
                                CommunicationsNewTimecardEntryAction
                                    { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                    , selectedMessageIds =
                                        [ "50000000-0000-0000-0000-000000000000"
                                        ]
                                    }
                            }
                        ]
                    , scheduledMessageItems =
                        [ Index.NotStartedScheduledMessageItem
                            { runsAt = "2021-06-23T22:30:00+0000"
                            , body = "Hello World!"
                            , editAction =
                                CommunicationsEditScheduledMessageAction
                                    { sendMessageActionId = "60000000-0000-0000-0000-000000000000"
                                    }
                            , updateAction =
                                CommunicationsUpdateScheduledMessageAction
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
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }
            let people = [person]

            let personSelection = Index.NoPersonSelected

            let currentColumn = Index.TimecardsColumn

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
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
                        , hoursWorked = 5.5
                        , workDone = "work done"
                        , invoiceTranslation = "invoice translation"
                        }

            let personActivity = Index.SendingMessage {timecards = []}

            let twilioMessage1 =
                    Twilio.View.Message
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
                        , entities = []
                        }

            let twilioMessage2 =
                    Twilio.View.Message
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
                        , entities = []
                        }

            Index.buildMessageItems
                person
                personActivity
                [twilioMessage1, twilioMessage2]
                `shouldBe` [ Index.MessageItem
                                { fromName = "Barbara Bush"
                                , sentAt = "2021-06-23T22:29:00+0000"
                                , entities = []
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
                                , sentAt = "2021-06-23T22:30:00+0000"
                                , entities = []
                                , statusClass = "message-status delivered"
                                , status = "Delivered"
                                , linkButtonActiveClass = ""
                                , linkButtonText = "Link"
                                , linkButtonAction =
                                    CommunicationsNewTimecardEntryAction
                                        { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                        , selectedMessageIds =
                                            [ "40000000-0000-0000-0000-000000000000"
                                            ]
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
                    Twilio.View.Message
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
                        , entities = []
                        }

            let twilioMessage2 =
                    Twilio.View.Message
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
                        , entities = []
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
                                , sentAt = "2021-06-23T22:29:00+0000"
                                , entities = []
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
                           , Index.MessageItem
                                { fromName = "Jackie Kennedy"
                                , sentAt = "2021-06-23T22:30:00+0000"
                                , entities = []
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
                    Twilio.View.Message
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
                        , entities =
                            [ Twilio.View.Entity
                                { entityType = Twilio.Query.JobName
                                , rawText = "barfed"
                                , confidence = 1.0
                                }
                            ]
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
                    , sentAt = "2021-06-23T22:29:00+0000"
                    , entities =
                        [ Index.RecognizedEntity
                            { classes = "entity job-name"
                            , content = "barfed"
                            , tooltip = "JobName: 100%"
                            }
                        ]
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
                    Twilio.View.Message
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
                        , entities = []
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
                    , sentAt = "2021-06-23T22:29:00+0000"
                    , entities = []
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

        it "returns a message item to unlink the message if it is currently linked" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #id "20000000-0000-0000-0000-000000000000"

            let twilioMessage =
                    Twilio.View.Message
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
                        , entities = []
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
                    , sentAt = "2021-06-23T22:29:00+0000"
                    , entities = []
                    , statusClass = "message-status delivered"
                    , status = "Delivered"
                    , linkButtonActiveClass = "active"
                    , linkButtonText = "Unlink"
                    , linkButtonAction =
                        CommunicationsNewTimecardEntryAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , selectedMessageIds =
                                [ "40000000-0000-0000-0000-000000000000"
                                ]
                            }
                    }

    describe "buildEntity" do
        it "returns a recognized entity when the entity type is recognized" do
            Index.buildEntity
                Twilio.View.Entity
                    { entityType = Twilio.Query.JobName
                    , rawText = "barfed"
                    , confidence = 1.0
                    }
                `shouldBe` Index.RecognizedEntity
                    { classes = "entity job-name"
                    , content = "barfed"
                    , tooltip = "JobName: 100%"
                    }
        it "returns an unrecognized entity when the entity type is not recognized" do
            Index.buildEntity
                Twilio.View.Entity
                    { entityType = Twilio.Query.Unrecognized
                    , rawText = "barfed"
                    , confidence = 1.0
                    }
                `shouldBe` Index.UnrecognizedEntity
                    { content = "barfed"
                    }

    describe "entityClass" do
        it "translates job name to css classes" do
            Index.entityClass Twilio.Query.JobName `shouldBe` "entity job-name"
        it "translates hours worked to css classes" do
            Index.entityClass Twilio.Query.HoursWorked `shouldBe` "entity hours-worked"
        it "translates clocked in at to css classes" do
            Index.entityClass Twilio.Query.ClockedInAt `shouldBe` "entity clocked-in-at"
        it "translates clocked out at to css classes" do
            Index.entityClass Twilio.Query.ClockedOutAt `shouldBe` "entity clocked-out-at"
        it "translates time on task to css classes" do
            Index.entityClass Twilio.Query.TimeOnTask `shouldBe` "entity time-on-task"
        it "translates work done to css classes" do
            Index.entityClass Twilio.Query.WorkDone `shouldBe` "entity work-done"
        it "translates unrecognized to css classes" do
            Index.entityClass Twilio.Query.Unrecognized `shouldBe` "entity unrecognized"

    describe "entityTooltip" do
        it "returns tooltip text to describe the entity" do
            Index.entityTooltip
                ( Twilio.View.Entity
                    { entityType = Twilio.Query.JobName
                    , rawText = "barfed"
                    , confidence = 1.0
                    }
                )
                `shouldBe` "JobName: 100%"

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
        it "returns a not started scheduled message item when the action is not started" do
            let sendMessageAction =
                    SendMessageAction.T
                        { id = "10000000-0000-0000-0000-000000000000"
                        , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                        , state = ActionRunState.notStarted
                        , runsAt = toUtc "2021-06-23 15:30:00 PDT"
                        , body = "Hello World!"
                        , fromId = "30000000-0000-0000-0000-000000000000"
                        , fromNumber = "+15555555555"
                        , toId = "40000000-0000-0000-0000-000000000000"
                        , toNumber = "+16666666666"
                        }

            Index.buildScheduledMessageItem
                Nothing
                "50000000-0000-0000-0000-000000000000"
                sendMessageAction
                `shouldBe` Index.NotStartedScheduledMessageItem
                    { runsAt = "2021-06-23T22:30:00+0000"
                    , body = "Hello World!"
                    , editAction =
                        CommunicationsEditScheduledMessageAction
                            { sendMessageActionId = "10000000-0000-0000-0000-000000000000"
                            }
                    , updateAction =
                        CommunicationsUpdateScheduledMessageAction
                            { sendMessageActionId = "10000000-0000-0000-0000-000000000000"
                            }
                    }

        it "returns a suspended scheduled message item when the action has been suspended" do
            let sendMessageAction =
                    SendMessageAction.T
                        { id = "10000000-0000-0000-0000-000000000000"
                        , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                        , state = ActionRunState.suspended
                        , runsAt = toUtc "2021-06-23 15:30:00 PDT"
                        , body = "Hello World!"
                        , fromId = "30000000-0000-0000-0000-000000000000"
                        , fromNumber = "+15555555555"
                        , toId = "40000000-0000-0000-0000-000000000000"
                        , toNumber = "+16666666666"
                        }

            Index.buildScheduledMessageItem
                Nothing
                "50000000-0000-0000-0000-000000000000"
                sendMessageAction
                `shouldBe` Index.SuspendedScheduledMessageItem
                    { runsAt = "2021-06-23T22:30:00+0000"
                    , body = "Hello World!"
                    , editAction =
                        CommunicationsEditScheduledMessageAction
                            { sendMessageActionId = "10000000-0000-0000-0000-000000000000"
                            }
                    , updateAction =
                        CommunicationsUpdateScheduledMessageAction
                            { sendMessageActionId = "10000000-0000-0000-0000-000000000000"
                            }
                    }

        it "returns an edit scheduled message form when we are editing a scheduled message" do
            let sendMessageAction =
                    SendMessageAction.T
                        { id = "10000000-0000-0000-0000-000000000000"
                        , actionRunStateId = "20000000-0000-0000-0000-000000000000"
                        , state = ActionRunState.suspended
                        , runsAt = toUtc "2021-06-23 15:30:00 PDT"
                        , body = "Hello World!"
                        , fromId = "30000000-0000-0000-0000-000000000000"
                        , fromNumber = "+15555555555"
                        , toId = "40000000-0000-0000-0000-000000000000"
                        , toNumber = "+16666666666"
                        }

            Index.buildScheduledMessageItem
                (Just "10000000-0000-0000-0000-000000000000")
                "50000000-0000-0000-0000-000000000000"
                sendMessageAction
                `shouldBe` Index.EditScheduledMessageForm
                    { runsAt = "2021-06-23T22:30:00+0000"
                    , body = "Hello World!"
                    , sendMessageActionId = "10000000-0000-0000-0000-000000000000"
                    , saveAction =
                        CommunicationsUpdateScheduledMessageAction
                            { sendMessageActionId = "10000000-0000-0000-0000-000000000000"
                            }
                    , cancelAction =
                        CommunicationsPersonSelectionAction
                            { selectedPersonId = "50000000-0000-0000-0000-000000000000"
                            , column = Just "messages"
                            }
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
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            let people = [person]

            let selectedPerson =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    Timecard.View.TimecardEntry
                        { id = "20000000-0000-0000-0000-000000000000"
                        , date = toDay "2021-06-23"
                        , jobName = "job name"
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
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
                        { selectedPerson = selectedPerson
                        , messages = []
                        , toPhoneNumber = newRecord @PhoneNumber
                        , scheduledMessages = []
                        , editingScheduledMessageId = Nothing
                        , newMessage = newRecord @TwilioMessage
                        , personActivity =
                            Index.SendingMessage
                                { timecards = [timecard]
                                }
                        }

            let currentColumn = Index.TimecardsColumn

            Index.buildTimecardsColumn Index.IndexView {..}
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
                                , deleteAction =
                                    CommunicationsDeleteTimecardEntryAction
                                        { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                                        , timecardEntryId = "20000000-0000-0000-0000-000000000000"
                                        }
                                }
                            ]
                        }
                    ]

        it "returns a timecard column with a timecard entry form when we are linking messages" do
            let person =
                    People.View.Person
                        { id = "10000000-0000-0000-0000-000000000000"
                        , firstName = "Barbara"
                        , lastName = "Bush"
                        , goesBy = "Barb"
                        , state = People.View.PersonIdle
                        }

            let people = [person]

            let selectedPerson =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #id "20000000-0000-0000-0000-000000000000"
                        |> set #date (toDay "2021-06-21")

            let twilioMessage =
                    Twilio.View.Message
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
                        , entities = []
                        }

            let personSelection =
                    Index.PersonSelected
                        { selectedPerson = selectedPerson
                        , messages = []
                        , toPhoneNumber = newRecord @PhoneNumber
                        , scheduledMessages = []
                        , editingScheduledMessageId = Nothing
                        , newMessage = newRecord @TwilioMessage
                        , personActivity =
                            Index.WorkingOnTimecardEntry
                                { timecardEntry = timecardEntry
                                , selectedMessages = [twilioMessage]
                                , timecardActivity = Index.CreatingEntry
                                }
                        }

            let currentColumn = Index.TimecardsColumn

            Index.buildTimecardsColumn Index.IndexView {..}
                `shouldBe` Index.EditTimecardEntry
                    { timecardEntryForm =
                        Index.TimecardEntryForm
                            { date = "2021-06-21"
                            , dateInvalidClass = ""
                            , dateError = Nothing
                            , jobName = ""
                            , jobNameInvalidClass = ""
                            , jobNameError = Nothing
                            , clockedInAt = ""
                            , clockedInAtInvalidClass = ""
                            , clockedInAtError = Nothing
                            , clockedOutAt = ""
                            , clockedOutAtInvalidClass = ""
                            , clockedOutAtError = Nothing
                            , lunchDuration = ""
                            , lunchDurationInvalidClass = ""
                            , lunchDurationError = Nothing
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
                                    , column = Just "timecards"
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
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
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
                            , deleteAction =
                                CommunicationsDeleteTimecardEntryAction
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
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
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
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
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
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
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
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
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
                        , clockedInAt = Nothing
                        , clockedOutAt = Nothing
                        , lunchDuration = Nothing
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
                    , deleteAction =
                        CommunicationsDeleteTimecardEntryAction
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
                    Twilio.View.Message
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
                        , entities = []
                        }

            let timecardActivity = Index.CreatingEntry

            let timecardEntry =
                    newRecord @TimecardEntry
                        |> set #date (toDay "2021-06-23")
                        |> set #jobName "McDonald's"
                        |> set #clockedInAt (Just $ toTimeOfDay "07:00:00")
                        |> set #clockedOutAt (Just $ toTimeOfDay "15:30:00")
                        |> set #lunchDuration (Just 30)
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
                    , clockedInAt = "07:00:00"
                    , clockedInAtInvalidClass = ""
                    , clockedInAtError = Nothing
                    , clockedOutAt = "15:30:00"
                    , clockedOutAtInvalidClass = ""
                    , clockedOutAtError = Nothing
                    , lunchDuration = "30"
                    , lunchDurationInvalidClass = ""
                    , lunchDurationError = Nothing
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
                            , column = "timecards"
                            }
                    }

        it "sets error fields when errors are present" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let twilioMessage =
                    Twilio.View.Message
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
                        , entities = []
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
                        |> validateField #clockedInAt (\_ -> Failure "clockedInAt error")
                        |> validateField #clockedOutAt (\_ -> Failure "clockedOutAt error")
                        |> validateField #lunchDuration (\_ -> Failure "lunchDuration error")
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
                    , dateError = Just $ TextViolation "date error"
                    , jobName = "McDonald's"
                    , jobNameInvalidClass = "is-invalid"
                    , jobNameError = Just $ TextViolation "jobName error"
                    , clockedInAt = ""
                    , clockedInAtInvalidClass = "is-invalid"
                    , clockedInAtError = Just $ TextViolation "clockedInAt error"
                    , clockedOutAt = ""
                    , clockedOutAtInvalidClass = "is-invalid"
                    , clockedOutAtError = Just $ TextViolation "clockedOutAt error"
                    , lunchDuration = ""
                    , lunchDurationInvalidClass = "is-invalid"
                    , lunchDurationError = Just $ TextViolation "lunchDuration error"
                    , hoursWorked = "8.0"
                    , hoursWorkedInvalidClass = "is-invalid"
                    , hoursWorkedError = Just $ TextViolation "hoursWorked error"
                    , workDone = "work"
                    , workDoneInvalidClass = "is-invalid"
                    , workDoneError = Just $ TextViolation "workDone error"
                    , invoiceTranslation = "invoice"
                    , invoiceTranslationInvalidClass = "is-invalid"
                    , invoiceTranslationError = Just $ TextViolation "invoiceTranslation error"
                    , selectedMessageIdsParam = "20000000-0000-0000-0000-000000000000"
                    , selectedPersonIdParam = "10000000-0000-0000-0000-000000000000"
                    , submitLabel = "Create"
                    , submitAction = CommunicationsCreateTimecardEntryAction
                    , cancelAction =
                        CommunicationsPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , column = Just "timecards"
                            }
                    }

        it "sets submit label and action to update when the timecard activity is editing" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let twilioMessage =
                    Twilio.View.Message
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
                        , entities = []
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
                    , clockedInAt = ""
                    , clockedInAtInvalidClass = ""
                    , clockedInAtError = Nothing
                    , clockedOutAt = ""
                    , clockedOutAtInvalidClass = ""
                    , clockedOutAtError = Nothing
                    , lunchDuration = ""
                    , lunchDurationInvalidClass = ""
                    , lunchDurationError = Nothing
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
                            , column = Just "timecards"
                            }
                    }

        it "concatenates message bodies in order of creation time" do
            let person =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let twilioMessage1 =
                    Twilio.View.Message
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
                        , entities = []
                        }
            let twilioMessage2 =
                    Twilio.View.Message
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
                        , entities = []
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
                    , clockedInAt = ""
                    , clockedInAtInvalidClass = ""
                    , clockedInAtError = Nothing
                    , clockedOutAt = ""
                    , clockedOutAtInvalidClass = ""
                    , clockedOutAtError = Nothing
                    , lunchDuration = ""
                    , lunchDurationInvalidClass = ""
                    , lunchDurationError = Nothing
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
                    , cancelAction =
                        CommunicationsPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , column = Just "timecards"
                            }
                    }

    describe "assembleMessageBodies" do
        it "returns the concatenated bodies of all of the twilio messages if the existing text is blank" do
            let twilioMessage1 =
                    Twilio.View.Message
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
                        , entities = []
                        }
            let twilioMessage2 =
                    Twilio.View.Message
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
                        , entities = []
                        }
            Index.assembleMessageBodies "" [twilioMessage1, twilioMessage2]
                `shouldBe` "What's up?\n\nNothing much."

        it "returns the existing text and ignores the messages if the existing text is not blank" do
            let twilioMessage1 =
                    Twilio.View.Message
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
                        , entities = []
                        }
            let twilioMessage2 =
                    Twilio.View.Message
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
                        , entities = []
                        }
            Index.assembleMessageBodies "Existing text" [twilioMessage1, twilioMessage2]
                `shouldBe` "Existing text"

    describe "buildColumnNavigation" do
        it "uses dark text for the currently selected column" do
            let selectedPerson =
                    newRecord @Person
                        |> set #id "10000000-0000-0000-0000-000000000000"

            let personSelection =
                    Index.PersonSelected
                        { selectedPerson = selectedPerson
                        , messages = []
                        , toPhoneNumber = newRecord @PhoneNumber
                        , scheduledMessages = []
                        , editingScheduledMessageId = Nothing
                        , newMessage = newRecord @TwilioMessage
                        , personActivity =
                            Index.SendingMessage
                                { timecards = []
                                }
                        }

            Index.buildColumnNavigation personSelection Index.PeopleColumn
                `shouldBe` Index.ColumnNavigation
                    { peopleLinkClass = "text-dark"
                    , peopleAction =
                        CommunicationsPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , column = Just "people"
                            }
                    , messagesLinkClass = "text-muted"
                    , messagesAction =
                        CommunicationsPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , column = Just "messages"
                            }
                    , timecardsLinkClass = "text-muted"
                    , timecardsAction =
                        CommunicationsPersonSelectionAction
                            { selectedPersonId = "10000000-0000-0000-0000-000000000000"
                            , column = Just "timecards"
                            }
                    }

    describe "columnToParam" do
        it "returns 'people' for the people column" do
            Index.columnToParam Index.PeopleColumn `shouldBe` "people"
        it "returns 'messages' for the people column" do
            Index.columnToParam Index.MessagesColumn `shouldBe` "messages"
        it "returns 'timecards' for the people column" do
            Index.columnToParam Index.TimecardsColumn `shouldBe` "timecards"
