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
                NoAnchor
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
            {renderSectionNavigation Timecards selectedPerson}

            <div class="d-flex">
                {renderPeopleNavigation peopleNavigation}
                {renderTimecardColumn timecardColumn}
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
                <div class="timecard-column m-3 flex-grow-1">
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
                            <th scope="col">Date</th>
                            <th scope="col">Job</th>
                            <th scope="col">Clock In</th>
                            <th scope="col">Clock Out</th>
                            <th scope="col">Lunch (mins)</th>
                            <th scope="col">Work Done</th>
                            <th scope="col">Invoice Translation</th>
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
            <td>{date}</td>
            <td>{jobName}</td>
            <td>{clockedInAt}</td>
            <td>{clockedOutAt}</td>
            <td>{lunchDuration}</td>
            <td class="work-done">{workDone}</td>
            {renderInvoiceTranslationCell invoiceTranslationCell}
            <td>{hoursWorked}</td>
        </tr>
    |]

renderInvoiceTranslationCell :: InvoiceTranslationCell -> Html
renderInvoiceTranslationCell invoiceTranslationCell =
    case invoiceTranslationCell of
        ShowInvoiceTranslation {..} ->
            [hsx|
                <td class="invoice-translation">
                    {invoiceTranslation} (<a href={editAction}>Edit</a>)
                </td>
            |]
        EditInvoiceTranslation {..} ->
            [hsx|
                <td class="invoice-translation">
                    <form method="POST" 
                        action={saveAction} id="edit-timecard-entry-form"
                        class="edit-form"
                        data-disable-javascript-submission="false">
                        
                        <div
                            class="form-group"
                            id="form-group-timecardEntry_invoiceTranslation">
                            <textarea
                                type="text"
                                name="invoiceTranslation"
                                placeholder=""
                                id="timecardEntry_invoiceTranslation"
                                class="form-control"
                                value={invoiceTranslation}>
                                {invoiceTranslation}
                            </textarea> 
                        </div>
                        
                        <div
                            class="form-group"
                            id="form-group-timecardEntry_id">
                            <input
                                type="hidden"
                                name="id"
                                placeholder=""
                                id="timecardEntry_id"
                                class="form-control"
                                value={timecardEntryId}>
                        </div>
                        
                        <button
                            class="btn btn-primary btn btn-primary btn-sm">
                            Save
                        </button>
                        
                        <a 
                            href={cancelAction}
                            class="btn btn-secondary btn-sm ml-2">
                            Cancel
                        </a>
                    </form>
                </td>
            |]

renderTotalHoursRow :: TotalHoursRow -> Html
renderTotalHoursRow TotalHoursRow {..} =
    [hsx|
        <tr class="table-active">
            <th scope="row" colspan="8">Total Hours</th>
            <td>{totalHours}</td>
        </tr>
    |]

styles :: Html
styles =
    [hsx|
        <style>
            :root {
                --top-nav-height: 7.25rem;
            }

            .people-list {
                height: calc(100vh - var(--top-nav-height));
                min-width: 18.75rem;
                overflow-y: scroll;
            }

            .timecard-column {
                height: calc(100vh - 150px);
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
                width: 300px;
            }

            .invoice-translation {
                width: 300px;
            }

            #timecardEntry_invoiceTranslation {
                font-size: .9rem;
                height: 150px;
            }
        </style>

        {removeScrollbars}
    |]
