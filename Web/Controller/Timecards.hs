module Web.Controller.Timecards where

import qualified Application.Base.People as People
import qualified Application.Service.Pdf as Pdf
import Application.Service.Time (parseDay)
import qualified Application.Timecard.Timecard as Timecard
import qualified Application.Timecard.TimecardEntry as TimecardEntry
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
        people <- People.fetchExcluding botId
        let personSelection = NoPersonSelected

        render IndexView {..}
    --
    action TimecardPersonSelectionAction {..} = do
        botId <- People.fetchBotId
        people <- People.fetchExcluding botId
        selectedPerson <- fetch selectedPersonId

        timecardEntries <- TimecardEntry.fetchByPerson selectedPersonId
        let timecards = Timecard.buildAll timecardEntries

        let personActivity = Viewing
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action TimecardEditTimecardEntryAction {..} = do
        botId <- People.fetchBotId
        people <- People.fetchExcluding botId

        selectedTimecardEntry <- fetch timecardEntryId
        selectedTimecard <- fetch (get #timecardId selectedTimecardEntry)
        let selectedPersonId = get #personId selectedTimecard
        selectedPerson <- fetch selectedPersonId

        timecardEntries <- TimecardEntry.fetchByPerson selectedPersonId
        let timecards = Timecard.buildAll timecardEntries

        let personActivity = Editing {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action TimecardDownloadTimecardAction {..} = do
        botId <- People.fetchBotId
        people <- People.fetchExcluding botId
        selectedPerson <- fetch selectedPersonId
        today <- utctDay <$> getCurrentTime

        let maybeDay = parseDay weekOf
        let day = fromMaybe today maybeDay

        timecardEntries <- TimecardEntry.fetchByPersonAndWeek selectedPersonId day
        let timecard = Timecard.buildForWeek day timecardEntries

        html <- renderHtml ShowPdfView {..}
        pdf <- Pdf.render html

        respondAndExit $ responseLBS status200 [(hContentType, "application/pdf")] pdf

    --
    action TimecardUpdateTimecardEntryAction = do
        let timecardEntryId = param @(Id TimecardEntry) "id"
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
                    people <- People.fetchExcluding botId

                    selectedPerson <- fetch selectedPersonId

                    timecardEntries <- TimecardEntry.fetchByPerson selectedPersonId
                    let timecards = Timecard.buildAll timecardEntries

                    let personActivity = Editing {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {..}
                Right timecardEntry -> do
                    updateRecord timecardEntry
                    redirectTo TimecardPersonSelectionAction {..}
