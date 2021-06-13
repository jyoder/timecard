module Web.Controller.Timecards where

import qualified Application.Base.People as People
import qualified Application.Timecard.Timecard as Timecard
import qualified Application.Timecard.TimecardEntry as TimecardEntry
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Set (empty)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Encoding as LT
import IHP.Log as Log
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header
import Network.Wai
import System.IO
import System.Process
import qualified Text.Blaze.Html.Renderer.Text as Blaze
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
        let selectedPersonId = get #personId selectedTimecardEntry
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

        let maybeDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" $ T.unpack weekOf
        let date = fromMaybe today maybeDay

        timecardEntries <- TimecardEntry.fetchByPerson selectedPersonId
        let timecard = Timecard.buildForWeek date timecardEntries

        bootstrap <- LBS.readFile "build/ihp-lib/static/vendor/bootstrap.min.css"
        html <- renderHtml ShowPdfView {..}
        let htmlText = Blaze.renderHtml html
        let htmlLt = LT.encodeUtf8 htmlText
        let fullHtml = "<html><head><style>" <> bootstrap <> "</style></head><body>" <> htmlLt <> "</body></html>"

        (Just hin, Just hout, _, _) <- createProcess (proc "wkhtmltopdf" ["-", "-"]) {std_in = CreatePipe, std_out = CreatePipe}
        LBS.hPut hin fullHtml
        hClose hin
        pdf <- LBS.hGetContents hout

        respondAndExit $ responseLBS status200 [(hContentType, "application/pdf")] pdf

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
                    let timecards = Timecard.buildAll timecardEntries

                    let personActivity = Editing {..}
                    let personSelection = PersonSelected {..}

                    render IndexView {..}
                Right timecardEntry -> do
                    updateRecord timecardEntry
                    redirectTo TimecardPersonSelectionAction {..}
