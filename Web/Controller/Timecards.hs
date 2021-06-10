module Web.Controller.Timecards where

import qualified Application.Base.People as People
import qualified Application.Timecard.Timecard as Timecard
import qualified Application.Timecard.TimecardEntry as TimecardEntry
import Text.Read (read)
import Web.Controller.Prelude
import Web.View.Timecards.Index

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
        let timecards = Timecard.buildAll companyTimeZone timecardEntries

        let personActivity = Viewing
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action TimecardEditTimecardEntryAction {..} = do
        botId <- People.fetchBotId
        people <- People.fetchExcluding botId

        selectedTimecardEntry <- fetch timecardEntryId
        let selectedPersonId = get #personId selectedTimecardEntry
        selectedPerson <- fetch selectedPersonId

        timecardEntries <- TimecardEntry.fetchByPerson selectedPersonId
        let timecards = Timecard.buildAll companyTimeZone timecardEntries

        let personActivity = Editing {..}
        let personSelection = PersonSelected {..}

        render IndexView {..}
    --
    action TimecardUpdateTimecardEntryAction = do
        let timecardEntryId = param @(Id TimecardEntry) "id"
        let selectedPersonId = param @(Id Person) "personId"
        let invoiceTranslation = param @Text "invoiceTranslation"

        timecardEntry <- fetch timecardEntryId
        timecardEntry
            |> set #invoiceTranslation invoiceTranslation
            |> validateField #invoiceTranslation nonEmpty
            |> ifValid \case
                Left selectedTimecardEntry -> do
                    botId <- People.fetchBotId
                    people <- People.fetchExcluding botId
                    selectedPerson <- fetch selectedPersonId

                    timecardEntries <- TimecardEntry.fetchByPerson selectedPersonId
                    let timecards = Timecard.buildAll companyTimeZone timecardEntries

                    let personActivity = Editing {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {..}
                Right timecardEntry -> do
                    updateRecord timecardEntry
                    redirectTo TimecardPersonSelectionAction {..}

companyTimeZone :: TimeZone
companyTimeZone = read "PDT"
