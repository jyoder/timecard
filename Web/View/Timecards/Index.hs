module Web.View.Timecards.Index where

import qualified Application.People.View as V
import qualified Application.Timecard.View as V
import Web.View.Navigation.People
import Web.View.Navigation.Section (Section (Timecards), renderSectionNavigation)
import Web.View.Prelude hiding (Page)
import Web.View.Service.Style (removeScrollbars)
import Web.View.Service.Time (formatDay, formatTimeOfDay)
import Web.View.Timecards.Status

data IndexView = IndexView
    { people :: ![V.Person]
    , personSelection :: !PersonSelection
    }

data PersonSelection
    = NoPersonSelected
    | PersonSelected
        { selectedPerson :: !Person
        , timecards :: ![V.Timecard]
        , personActivity :: !PersonActivity
        }

data PersonActivity
    = Viewing
    | EditingInvoiceTranslation
        { selectedTimecardEntry :: !TimecardEntry
        }

data Page = Page
    { selectedPerson :: !(Maybe Person)
    , peopleNavigation :: !(PeopleNavigation TimecardsController)
    , timecardColumn :: !TimecardColumn
    }
    deriving (Eq, Show)

data TimecardColumn
    = TimecardColumnNotVisible
    | TimecardColumnVisible
        { timecardTables :: ![TimecardTable]
        }
    deriving (Eq, Show)

data TimecardTable = TimecardTable
    { weekOf :: !Text
    , status :: !TimecardStatus
    , firstName :: !Text
    , lastName :: !Text
    , jobRows :: ![JobRow]
    , totalHoursRow :: !TotalHoursRow
    , downloadAction :: !TimecardsController
    , downloadFileName :: !Text
    }
    deriving (Eq, Show)

data JobRow = JobRow
    { dayOfWeek' :: !Text
    , date :: !Text
    , jobName :: !Text
    , clockedInAt :: !Text
    , clockedOutAt :: !Text
    , lunchDuration :: !Text
    , hoursWorked :: !Text
    , workDone :: !Text
    , invoiceTranslationCell :: !InvoiceTranslationCell
    }
    deriving (Eq, Show)

newtype TotalHoursRow = TotalHoursRow
    { totalHours :: Text
    }
    deriving (Eq, Show)

data InvoiceTranslationCell
    = ShowInvoiceTranslation
        { invoiceTranslation :: !Text
        , editAction :: !TimecardsController
        }
    | EditInvoiceTranslation
        { invoiceTranslation :: !Text
        , timecardEntryId :: !Text
        , saveAction :: !TimecardsController
        , cancelAction :: !TimecardsController
        }
    deriving (Eq, Show)

data Column
    = PeopleColumn
    | TimecardsColumn
    deriving (Eq, Show)

instance View IndexView where
    html view = renderPage $ buildPage view

buildPage :: IndexView -> Page
buildPage view =
    Page
        { selectedPerson = selectedPerson
        , peopleNavigation =
            buildPeopleNavigation
                BadgesHidden
                TimecardPersonSelectionAction
                selectedPerson
                (get #people view)
        , timecardColumn = buildTimecardColumn view
        }
  where
    selectedPerson = case get #personSelection view of
        NoPersonSelected -> Nothing
        PersonSelected {..} -> Just selectedPerson

buildTimecardColumn :: IndexView -> TimecardColumn
buildTimecardColumn IndexView {..} =
    case personSelection of
        NoPersonSelected -> TimecardColumnNotVisible
        PersonSelected {..} ->
            TimecardColumnVisible
                { timecardTables =
                    buildTimecardTable
                        selectedPerson
                        personActivity
                        <$> timecards
                }

buildTimecardTable :: Person -> PersonActivity -> V.Timecard -> TimecardTable
buildTimecardTable selectedPerson personActivity timecard =
    TimecardTable
        { weekOf = formatDay $ get #weekOf timecard
        , status = timecardStatus $ get #status timecard
        , firstName = firstName
        , lastName = lastName
        , jobRows =
            buildJobRow selectedPerson personActivity
                <$> get #entries timecard
        , totalHoursRow = buildTotalHoursRow $ get #entries timecard
        , downloadAction = downloadAction
        , downloadFileName = downloadFileName
        }
  where
    lastName = get #lastName selectedPerson
    firstName = get #firstName selectedPerson
    downloadAction = TimecardDownloadTimecardAction $ get #id timecard
    downloadFileName = showWeekOf <> "-" <> lastName <> "-" <> firstName <> ".pdf"
    showWeekOf = show weekOf
    weekOf = get #weekOf timecard

buildJobRow :: Person -> PersonActivity -> V.TimecardEntry -> JobRow
buildJobRow selectedPerson personActivity timecardEntry =
    JobRow
        { dayOfWeek' = show $ dayOfWeek $ get #date timecardEntry
        , date = formatDay $ get #date timecardEntry
        , jobName = get #jobName timecardEntry
        , clockedInAt = maybe "--" formatTimeOfDay (get #clockedInAt timecardEntry)
        , clockedOutAt = maybe "--" formatTimeOfDay (get #clockedOutAt timecardEntry)
        , lunchDuration = maybe "--" show (get #lunchDuration timecardEntry)
        , hoursWorked = show $ get #hoursWorked timecardEntry
        , workDone = get #workDone timecardEntry
        , invoiceTranslationCell =
            buildInvoiceTranslationCell
                selectedPerson
                personActivity
                timecardEntry
        }

buildInvoiceTranslationCell ::
    Person ->
    PersonActivity ->
    V.TimecardEntry ->
    InvoiceTranslationCell
buildInvoiceTranslationCell
    selectedPerson
    personActivity
    timecardEntry =
        case personActivity of
            Viewing ->
                ShowInvoiceTranslation
                    { invoiceTranslation = get #invoiceTranslation timecardEntry
                    , editAction = TimecardEditTimecardEntryAction $ get #id timecardEntry
                    }
            EditingInvoiceTranslation {..} ->
                if get #id timecardEntry == get #id selectedTimecardEntry
                    then
                        EditInvoiceTranslation
                            { invoiceTranslation = get #invoiceTranslation timecardEntry
                            , timecardEntryId = show $ get #id timecardEntry
                            , saveAction = TimecardUpdateTimecardEntryAction $ get #id timecardEntry
                            , cancelAction = TimecardPersonSelectionAction $ get #id selectedPerson
                            }
                    else
                        ShowInvoiceTranslation
                            { invoiceTranslation = get #invoiceTranslation timecardEntry
                            , editAction = TimecardEditTimecardEntryAction $ get #id timecardEntry
                            }

buildTotalHoursRow :: [V.TimecardEntry] -> TotalHoursRow
buildTotalHoursRow timecardEntries =
    TotalHoursRow
        { totalHours = show $ sum $ get #hoursWorked <$> timecardEntries
        }

renderPage :: Page -> Html
renderPage Page {..} =
    [hsx|
            <div class="d-none d-xl-block">
                {renderSectionNavigation Timecards selectedPerson}

                <div class="d-flex flex-row">
                    {renderPeopleNavigation peopleNavigation}
                    {renderTimecardColumn timecardColumn}
                </div>
            </div>

            <div class="d-block d-xl-none">
                <div class="d-flex flex-column">
                    <div id="people" class="d-flex flex-column">
                        {renderSectionNavigation Timecards selectedPerson}
                        {renderPeopleNavigation peopleNavigation}
                        {renderColumnNavigation PeopleColumn}
                        <div class="browser-nav bg-light"></div>
                    </div>
                    <div id="timecards" class="d-flex flex-column">
                        {renderSectionNavigation Timecards selectedPerson}
                        {renderTimecardColumn timecardColumn}
                        {renderColumnNavigation TimecardsColumn}
                        <div class="browser-nav bg-light"></div>
                    </div>
                </div>
            </div>

            {styles}
        |]

renderTimecardColumn :: TimecardColumn -> Html
renderTimecardColumn timecardColumn =
    case timecardColumn of
        TimecardColumnNotVisible ->
            [hsx||]
        TimecardColumnVisible {..} ->
            [hsx|
                <div class="timecards-column m-xl-3 flex-grow-1">
                    {forEach timecardTables renderTimecardTable}
                </div>
            |]

renderTimecardTable :: TimecardTable -> Html
renderTimecardTable TimecardTable {..} =
    [hsx|
        <div class="card mb-5">
            <div class="card-header d-flex justify-content-start mb-2">
                <h5>Week Of {weekOf}</h5>
                <div class="ml-2">
                    {renderTimecardStatus status}
                </div>
            </div>
            
            <div class="card-body">
                <h5 class="card-title">
                    {lastName}, {firstName}
                </h5>

                <table class="table sticky-header">
                    <thead>
                        <tr>
                            <th scope="col">Day</th>
                            <th scope="col" class="d-none d-md-table-cell">Date</th>
                            <th scope="col">Job</th>
                            <th scope="col" class="d-none d-md-table-cell">Clock In</th>
                            <th scope="col" class="d-none d-md-table-cell">Clock Out</th>
                            <th scope="col" class="d-none d-md-table-cell">Lunch (mins)</th>
                            <th scope="col" class="d-none d-md-table-cell">Work Done</th>
                            <th scope="col" class="d-none d-md-table-cell">Invoice Translation</th>
                            <th scope="col">Hours</th>
                        </tr>
                    </thead>
                    <tbody>
                        {forEach jobRows renderJobRow}
                        {renderTotalHoursRow totalHoursRow}
                    </tbody>
                </table>
                <a href={downloadAction} download={downloadFileName}>Download PDF</a>
            </div>
        </div>
    |]

renderTimecardStatus :: TimecardStatus -> Html
renderTimecardStatus TimecardStatus {..} =
    [hsx|
        <span class={statusClasses}>
            {statusLabel}
        </span>
    |]

renderJobRow :: JobRow -> Html
renderJobRow JobRow {..} =
    [hsx|
        <tr>
            <th scope="row">{dayOfWeek'}</th>
            <td class="d-none d-md-table-cell">{date}</td>
            <td>{jobName}</td>
            <td class="d-none d-md-table-cell">{clockedInAt}</td>
            <td class="d-none d-md-table-cell">{clockedOutAt}</td>
            <td class="d-none d-md-table-cell">{lunchDuration}</td>
            <td class="work-done d-none d-md-table-cell">{workDone}</td>
            {renderInvoiceTranslationCell invoiceTranslationCell}
            <td>{hoursWorked}</td>
        </tr>
    |]

renderInvoiceTranslationCell :: InvoiceTranslationCell -> Html
renderInvoiceTranslationCell invoiceTranslationCell =
    case invoiceTranslationCell of
        ShowInvoiceTranslation {..} ->
            [hsx|
                <td class="invoice-translation d-none d-md-table-cell">
                    {invoiceTranslation} (<a href={editAction}>Edit</a>)
                </td>
            |]
        EditInvoiceTranslation {..} ->
            [hsx|
                <td class="invoice-translation d-none d-md-table-cell">
                    <form method="POST" action={saveAction} class="edit-form" data-disable-javascript-submission="false">
                        <div class="form-group">
                            <textarea type="text" name="invoiceTranslation" placeholder="" class="invoice-translation-input form-control" value={invoiceTranslation}>
                                {invoiceTranslation}
                            </textarea> 
                        </div>
                        
                        <button class="btn btn-primary btn btn-primary btn-sm">Save</button>
                        <a href={cancelAction} class="btn btn-secondary btn-sm ml-2">Cancel</a>
                    </form>
                </td>
            |]

renderTotalHoursRow :: TotalHoursRow -> Html
renderTotalHoursRow TotalHoursRow {..} =
    [hsx|
        <tr class="table-active">
            <th scope="row">Total Hours</th>
            <td class="d-none d-md-table-cell"></td>
            <td></td>
            <td class="d-none d-md-table-cell"></td>
            <td class="d-none d-md-table-cell"></td>
            <td class="d-none d-md-table-cell"></td>
            <td class="d-none d-md-table-cell"></td>
            <td class="d-none d-md-table-cell"></td>
            <td>{totalHours}</td>
        </tr>
    |]

renderColumnNavigation :: Column -> Html
renderColumnNavigation currentColumn =
    [hsx|
        <ul class="bottom-nav m-0 p-0 border-top bg-light d-flex">
            <li class="bottom-nav-item flex-even border-right d-flex justify-content-center">
                <a class={"bottom-nav-link text-center " <> linkClass PeopleColumn} href="#people">People</a>
            </li>
            <li class="bottom-nav-item flex-even border-left d-flex justify-content-center">
                <a class={"bottom-nav-link text-center " <> linkClass TimecardsColumn} href="#timecards">Timecards</a>
            </li>
        </ul>
    |]
  where
    linkClass column = if column == currentColumn then "text-dark" :: Text else "text-muted"

styles :: Html
styles =
    [hsx|
        <style>
            @media only screen and (min-width: 1200px) {
                :root {
                    --bottom-nav-height: 0rem;
                    --browser-nav-height: 0rem;
                }
            }

            @media only screen and (max-width: 1200px) {
                :root {
                    --bottom-nav-height: 3rem;
                    --browser-nav-height: 7rem;
                }
            }

            :root {
                --top-nav-height: 7.25rem;
                --total-nav-height: calc(var(--top-nav-height) + var(--bottom-nav-height) + var(--browser-nav-height));
                --screen-height: 100vh;
            }

            .bottom-nav {
                height: var(--bottom-nav-height);
            }

            .bottom-nav-item {
                list-style-type: none;
            }

            .bottom-nav-link {
                width: 100%;
                line-height: 2.5rem;
            }

            .browser-nav {
                height: var(--browser-nav-height);
            }

            .people-list {
                height: calc(var(--screen-height) - var(--total-nav-height));
                min-width: 18.75rem;
                overflow-y: scroll;
            }

            .timecards-column {
                height: calc(var(--screen-height) - var(--total-nav-height));
                overflow-y: scroll;
                font-size: .9rem;
            }

            .sticky-header thead th { 
                position: sticky;
                top: 0;
                z-index: 1;
                background: white;
                border: none;
            }

            .work-done {
                width: 18.75rem;
            }

            .invoice-translation {
                width: 18.75rem;
            }

            .invoice-translation-input.form-control {
                font-size: .9rem;
                height: 9.4rem;
            }

            .flex-even {
                flex: 1;
            }

            body {
                overflow: hidden;
            }
        </style>

        {removeScrollbars}
    |]
