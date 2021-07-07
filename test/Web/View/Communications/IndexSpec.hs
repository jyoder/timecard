module Web.View.Communications.IndexSpec where

import qualified Application.Timecard.View as Timecard.View
import qualified Application.Twilio.Query as Twilio.Query
import Generated.Types
import IHP.ControllerPrelude
import Test.Hspec
import Text.Read (read)
import Web.Types
import qualified Web.View.Communications.Index as Index

spec :: Spec
spec = do
    describe "buildTimecardEntryCard" $ do
        it "returns a timecard entry card based on the given parameters" $ do
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

    describe "buildTimecardEntryForm" $ do
        it "sets error fields to nothing when no errors are present" $ do
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
                    , cancelAction = CommunicationsPersonSelectionAction {selectedPersonId = "10000000-0000-0000-0000-000000000000"}
                    }

        it "sets error fields when errors are present" $ do
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
                    , cancelAction = CommunicationsPersonSelectionAction {selectedPersonId = "10000000-0000-0000-0000-000000000000"}
                    }

        it "sets submit label and action to update when the timecard activity is editing" $ do
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
                    , submitAction = CommunicationsUpdateTimecardEntryAction {timecardEntryId = "30000000-0000-0000-0000-000000000000"}
                    , cancelAction = CommunicationsPersonSelectionAction {selectedPersonId = "10000000-0000-0000-0000-000000000000"}
                    }

        it "concatenates message bodies in order of creation time" $ do
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

    describe "assembleMessageBodies" $ do
        it "returns the concatenated bodies of all of the twilio messages if the existing text is blank" $ do
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

        it "returns the existing text and ignores the messages if the existing text is not blank" $ do
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

toUtc :: String -> UTCTime
toUtc time = zonedTimeToUTC (read time :: ZonedTime)

toDay :: String -> Day
toDay = read