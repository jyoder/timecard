module Web.Controller.Timecards where

import qualified Application.People.Person as Person
import qualified Application.People.Query as People.Query
import qualified Application.People.View as People.View
import qualified Application.Service.Pdf as Pdf
import Application.Service.Time (parseDay)
import qualified Application.Timecard.Entry as Timecard.Entry
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Timecard.View as Timecard.View
import Data.Maybe (fromJust)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (responseLBS)
import Text.Read (read)
import Web.Controller.Prelude
import Web.View.Timecards.Index
import Web.View.Timecards.ShowPdf

instance Controller TimecardsController where
    beforeAction = ensureIsUser

    action TimecardsAction = do
        let jumpToTop = True
        let currentColumn = PeopleColumn
        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers

        personSelection <- case people of
            firstPerson : _ -> do
                let selectedPersonId = get #id firstPerson
                selectedPerson <- fetch selectedPersonId

                timecards <-
                    Timecard.View.buildTimecards
                        <$> Timecard.Query.fetchByPerson
                            Timecard.Query.EntriesDateDescending
                            selectedPersonId

                let personActivity = Viewing
                let personSelection = PersonSelected {..}
                pure PersonSelected {..}
            [] -> pure NoPersonSelected

        render IndexView {..}
    --
    action TimecardPersonSelectionAction {..} = do
        let currentColumn = maybe PeopleColumn paramToColumn column
        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers
        selectedPerson <- fetch selectedPersonId

        timecards <-
            Timecard.View.buildTimecards
                <$> Timecard.Query.fetchByPerson
                    Timecard.Query.EntriesDateAscending
                    selectedPersonId

        let personActivity = Viewing
        let personSelection = PersonSelected {..}

        render IndexView {jumpToTop = jumpToTop == Just 1, ..}
    --
    action TimecardEditTimecardEntryAction {..} = do
        let jumpToTop = False
        let currentColumn = TimecardsColumn
        let editingField = paramToEditableField $ param "editingField"

        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers

        selectedTimecardEntry <- fetch timecardEntryId
        selectedTimecard <- fetch (get #timecardId selectedTimecardEntry)
        let selectedPersonId = get #personId selectedTimecard
        selectedPerson <- fetch selectedPersonId

        timecards <-
            Timecard.View.buildTimecards
                <$> Timecard.Query.fetchByPerson
                    Timecard.Query.EntriesDateAscending
                    selectedPersonId

        let personActivity = Editing {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action TimecardDownloadTimecardAction {..} = do
        let currentColumn = TimecardsColumn
        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers
        timecard <-
            fromJust . Timecard.View.buildTimecard
                <$> Timecard.Query.fetchById
                    Timecard.Query.EntriesDateAscending
                    timecardId
        selectedPerson <- fetch (get #personId timecard)

        maybeTimecardSigning <-
            query @TimecardSigning
                |> filterWhere (#timecardId, get #id timecard)
                |> fetchOneOrNothing

        signing <- case maybeTimecardSigning of
            Just timecardSigning ->
                query @Signing
                    |> filterWhere (#id, get #signingId timecardSigning)
                    |> fetchOneOrNothing
            Nothing -> pure Nothing

        html <- renderHtml ShowPdfView {..}
        pdf <- Pdf.render html

        respondAndExit $ responseLBS status200 [(hContentType, "application/pdf")] pdf

    --
    action TimecardUpdateTimecardEntryAction {..} = do
        let currentColumn = TimecardsColumn
        let column = Just $ columnToParam currentColumn
        let editingField = paramToEditableField $ param "editingField"

        timecardEntry <- fetch timecardEntryId
        timecard <- fetch (get #timecardId timecardEntry)
        let selectedPersonId = get #personId timecard

        let timecardEntry' = case editingField of
                JobNameField ->
                    timecardEntry |> fill @["jobName", "jobName"]
                ClockedInAtField ->
                    timecardEntry |> fill @["clockedInAt", "clockedInAt"]
                ClockedOutAtField ->
                    timecardEntry |> fill @["clockedOutAt", "clockedOutAt"]
                LunchDurationField ->
                    timecardEntry |> fill @["lunchDuration", "lunchDuration"]
                InvoiceTranslationField ->
                    timecardEntry |> fill @["invoiceTranslation", "invoiceTranslation"]
                HoursWorkedField ->
                    timecardEntry |> fill @["hoursWorked", "hoursWorked"]

        timecardEntry'
            |> Timecard.Entry.validate
            >>= ifValid \case
                Left selectedTimecardEntry -> do
                    people <-
                        People.View.buildPeople
                            <$> People.Query.fetchActiveWorkers
                    selectedPerson <- fetch selectedPersonId

                    timecards <-
                        Timecard.View.buildTimecards
                            <$> Timecard.Query.fetchByPerson
                                Timecard.Query.EntriesDateAscending
                                selectedPersonId

                    let jumpToTop = False
                    let personActivity = Editing {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {..}
                Right timecardEntry -> do
                    updateRecord timecardEntry
                    let jumpToTop = Nothing
                    redirectTo TimecardPersonSelectionAction {..}

paramToColumn :: Text -> Column
paramToColumn "people" = PeopleColumn
paramToColumn "timecards" = TimecardsColumn
paramToColumn _ = PeopleColumn

paramToEditableField :: Text -> EditableField
paramToEditableField "jobName" = JobNameField
paramToEditableField "clockedInAt" = ClockedInAtField
paramToEditableField "clockedOutAt" = ClockedOutAtField
paramToEditableField "lunchDuration" = LunchDurationField
paramToEditableField "invoiceTranslation" = InvoiceTranslationField
paramToEditableField "hoursWorked" = HoursWorkedField
paramToEditableField _ = JobNameField