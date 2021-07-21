module Web.Controller.Timecards where

import qualified Application.Base.People as People
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
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        let personSelection = NoPersonSelected

        render IndexView {..}
    --
    action TimecardPersonSelectionAction {..} = do
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
        selectedPerson <- fetch selectedPersonId

        timecards <-
            Timecard.View.buildTimecards
                <$> Timecard.Query.fetchByPerson
                    Timecard.Query.EntriesDateAscending
                    selectedPersonId

        let personActivity = Viewing
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action TimecardEditTimecardEntryAction {..} = do
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId

        selectedTimecardEntry <- fetch timecardEntryId
        selectedTimecard <- fetch (get #timecardId selectedTimecardEntry)
        let selectedPersonId = get #personId selectedTimecard
        selectedPerson <- fetch selectedPersonId

        timecards <-
            Timecard.View.buildTimecards
                <$> Timecard.Query.fetchByPerson
                    Timecard.Query.EntriesDateAscending
                    selectedPersonId

        let personActivity = EditingInvoiceTranslation {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action TimecardDownloadTimecardAction {..} = do
        botId <- People.fetchBotId
        people <- People.fetchExcludingId botId
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
        let invoiceTranslation = param @Text "invoiceTranslation"

        timecardEntry <- fetch timecardEntryId
        timecard <- fetch (get #timecardId timecardEntry)
        let selectedPersonId = get #personId timecard

        timecardEntry
            |> set #invoiceTranslation invoiceTranslation
            |> validateField #invoiceTranslation nonEmpty
            |> ifValid \case
                Left selectedTimecardEntry -> do
                    botId <- People.fetchBotId
                    people <- People.fetchExcludingId botId
                    selectedPerson <- fetch selectedPersonId

                    timecards <-
                        Timecard.View.buildTimecards
                            <$> Timecard.Query.fetchByPerson
                                Timecard.Query.EntriesDateAscending
                                selectedPersonId

                    let personActivity = EditingInvoiceTranslation {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {..}
                Right timecardEntry -> do
                    updateRecord timecardEntry
                    redirectTo TimecardPersonSelectionAction {..}
