module Web.View.Timecards.Index where

import qualified Application.People.View as V
import qualified Application.Timecard.View as V
import qualified Text.Blaze.Html5 as Html5
import Web.View.Navigation.People (BadgeVisibility (..), PeopleNavigation, buildPeopleNavigation, renderPeopleNavigation)
import Web.View.Navigation.Section (Section (Timecards), renderSectionNavigation)
import Web.View.Prelude hiding (Page)
import Web.View.Service.Style (removeScrollbars)
import Web.View.Service.Time (formatDay, formatTimeOfDay)
import Web.View.Timecards.Status

data IndexView = IndexView
    { people :: ![V.Person]
    , personSelection :: !PersonSelection
    , currentColumn :: !Column
    , jumpToTop :: !Bool
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
    | Editing
        { selectedTimecardEntry :: !TimecardEntry
        , editingField :: !EditableField
        }

data Page = Page
    { selectedPerson :: !(Maybe Person)
    , peopleNavigationClasses :: !Text
    , peopleNavigation :: !(PeopleNavigation TimecardsController)
    , timecardsColumnClasses :: !Text
    , timecardsColumn :: !TimecardsColumn
    , columnNavigation :: !ColumnNavigation
    }
    deriving (Eq, Show)

data TimecardsColumn
    = TimecardsColumnNotVisible
    | TimecardsColumnVisible
        { timecardTables :: ![TimecardTable]
        , jumpToTopClass :: !Text
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
    , jobNameCell :: !TableCell
    , clockedInAtCell :: !TableCell
    , clockedOutAtCell :: !TableCell
    , lunchDurationCell :: !TableCell
    , workDoneCell :: !TableCell
    , invoiceTranslationCell :: !TableCell
    , hoursWorkedCell :: !TableCell
    }
    deriving (Eq, Show)

newtype TotalHoursRow = TotalHoursRow
    { totalHours :: Text
    }
    deriving (Eq, Show)

data TableCell
    = ShowCell
        { editableField :: !EditableField
        , value :: !Text
        , editAction :: !TimecardsController
        }
    | EditCell
        { editableField :: !EditableField
        , value :: !Text
        , timecardEntryId :: !Text
        , saveAction :: !TimecardsController
        , cancelAction :: !TimecardsController
        }
    deriving (Eq, Show)

data ColumnNavigation = ColumnNavigation
    { peopleLinkClass :: !Text
    , peopleAction :: !TimecardsController
    , timecardsLinkClass :: !Text
    , timecardsAction :: !TimecardsController
    }
    deriving (Eq, Show)

data Column
    = PeopleColumn
    | TimecardsColumn
    deriving (Eq, Show)

data EditableField
    = JobNameField
    | ClockedInAtField
    | ClockedOutAtField
    | LunchDurationField
    | WorkDoneField
    | InvoiceTranslationField
    | HoursWorkedField
    deriving (Eq, Show)

instance View IndexView where
    html view = renderPage $ buildPage view

buildPage :: IndexView -> Page
buildPage view =
    let IndexView {..} = view
     in Page
            { selectedPerson = selectedPerson
            , peopleNavigationClasses = columnClasses PeopleColumn currentColumn
            , peopleNavigation =
                buildPeopleNavigation
                    BadgesHidden
                    ( \selectedPersonId ->
                        TimecardPersonSelectionAction
                            { selectedPersonId
                            , column = Just $ columnToParam TimecardsColumn
                            , jumpToTop = Just 1
                            }
                    )
                    selectedPerson
                    people
            , timecardsColumnClasses = columnClasses TimecardsColumn currentColumn
            , timecardsColumn = buildTimecardsColumn view
            , columnNavigation = buildColumnNavigation personSelection currentColumn
            }
  where
    selectedPerson = case get #personSelection view of
        NoPersonSelected -> Nothing
        PersonSelected {..} -> Just selectedPerson

columnClasses :: Column -> Column -> Text
columnClasses column currentColumn =
    if currentColumn == column
        then "d-flex flex-grow-1 flex-lg-grow-0"
        else "d-none d-lg-flex"

buildTimecardsColumn :: IndexView -> TimecardsColumn
buildTimecardsColumn IndexView {..} =
    case personSelection of
        NoPersonSelected -> TimecardsColumnNotVisible
        PersonSelected {..} ->
            TimecardsColumnVisible
                { jumpToTopClass = jumpToTopClass'
                , timecardTables =
                    buildTimecardTable
                        selectedPerson
                        personActivity
                        <$> timecards
                }
  where
    jumpToTopClass' = if jumpToTop then "scroll-to-pinned" else ""

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
        , jobNameCell =
            buildTableCell
                selectedPerson
                personActivity
                JobNameField
                timecardEntry
                (get #jobName timecardEntry)
                (get #jobName timecardEntry)
        , clockedInAtCell =
            buildTableCell
                selectedPerson
                personActivity
                ClockedInAtField
                timecardEntry
                (maybe "" formatTimeOfDay (get #clockedInAt timecardEntry))
                (maybe "" show (get #clockedInAt timecardEntry))
        , clockedOutAtCell =
            buildTableCell
                selectedPerson
                personActivity
                ClockedOutAtField
                timecardEntry
                (maybe "" formatTimeOfDay (get #clockedOutAt timecardEntry))
                (maybe "" show (get #clockedOutAt timecardEntry))
        , lunchDurationCell =
            buildTableCell
                selectedPerson
                personActivity
                LunchDurationField
                timecardEntry
                (maybe "" show (get #lunchDuration timecardEntry))
                (maybe "" show (get #lunchDuration timecardEntry))
        , workDoneCell =
            buildTableCell
                selectedPerson
                personActivity
                WorkDoneField
                timecardEntry
                (get #workDone timecardEntry)
                (get #workDone timecardEntry)
        , invoiceTranslationCell =
            buildTableCell
                selectedPerson
                personActivity
                InvoiceTranslationField
                timecardEntry
                (get #invoiceTranslation timecardEntry)
                (get #invoiceTranslation timecardEntry)
        , hoursWorkedCell =
            buildTableCell
                selectedPerson
                personActivity
                HoursWorkedField
                timecardEntry
                (show $ get #hoursWorked timecardEntry)
                (show $ get #hoursWorked timecardEntry)
        }

buildTableCell ::
    Person ->
    PersonActivity ->
    EditableField ->
    V.TimecardEntry ->
    Text ->
    Text ->
    TableCell
buildTableCell
    selectedPerson
    personActivity
    editableField
    timecardEntry
    showValue
    editValue =
        case personActivity of
            Editing {..} ->
                if (get #id timecardEntry == get #id selectedTimecardEntry)
                    && editingField == editableField
                    then
                        EditCell
                            { editableField
                            , value = editValue
                            , timecardEntryId = show $ get #id timecardEntry
                            , saveAction =
                                TimecardUpdateTimecardEntryAction
                                    { timecardEntryId = get #id timecardEntry
                                    , editingField = editableFieldToParam editableField
                                    }
                            , cancelAction =
                                TimecardPersonSelectionAction
                                    { selectedPersonId = get #id selectedPerson
                                    , column = Just $ columnToParam TimecardsColumn
                                    , jumpToTop = Nothing
                                    }
                            }
                    else
                        ShowCell
                            { editableField
                            , value = showValue
                            , editAction =
                                TimecardEditTimecardEntryAction
                                    { timecardEntryId = get #id timecardEntry
                                    , editingField = editableFieldToParam editableField
                                    }
                            }
            _ ->
                ShowCell
                    { editableField
                    , value = showValue
                    , editAction =
                        TimecardEditTimecardEntryAction
                            { timecardEntryId = get #id timecardEntry
                            , editingField = editableFieldToParam editableField
                            }
                    }

buildTotalHoursRow :: [V.TimecardEntry] -> TotalHoursRow
buildTotalHoursRow timecardEntries =
    TotalHoursRow
        { totalHours = show $ sum $ get #hoursWorked <$> timecardEntries
        }

buildColumnNavigation :: PersonSelection -> Column -> ColumnNavigation
buildColumnNavigation personSelection currentColumn =
    ColumnNavigation
        { peopleLinkClass = linkClass PeopleColumn
        , peopleAction = action PeopleColumn
        , timecardsLinkClass = linkClass TimecardsColumn
        , timecardsAction = action TimecardsColumn
        }
  where
    linkClass column = if column == currentColumn then "text-dark" else "text-muted"
    action column = case personSelection of
        NoPersonSelected -> TimecardsAction
        PersonSelected {..} ->
            TimecardPersonSelectionAction
                { selectedPersonId = get #id selectedPerson
                , column = Just $ columnToParam column
                , jumpToTop = Just 1
                }

renderPage :: Page -> Html
renderPage Page {..} =
    [hsx|
        <div class="d-flex flex-column">
            {renderSectionNavigation Timecards selectedPerson}
            {renderColumnNavigation columnNavigation}
            
            <div class="d-flex flex-row">
                <div class={"mr-lg-3 flex-column flex-shrink-1 " <> peopleNavigationClasses}>
                    {renderPeopleNavigation peopleNavigation}
                </div>
                <div class={"flex-column " <> timecardsColumnClasses}>
                    {renderTimecardsColumn timecardsColumn}
                </div>
            </div>
        </div>

        {styles}
    |]

renderTimecardsColumn :: TimecardsColumn -> Html
renderTimecardsColumn timecardsColumn =
    case timecardsColumn of
        TimecardsColumnNotVisible ->
            [hsx||]
        TimecardsColumnVisible {..} ->
            [hsx|
                <div class="timecards-column mr-lg-3">
                    <div class={jumpToTopClass <>  " d-none d-lg-block"}></div>
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

                <div class="table-responsive-lg">
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
                </div>
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
            <th scope="row" class="day-of-week">{dayOfWeek'}</th>
            <td scope="col">{date}</td>
            {renderTableCell jobNameCell}
            {renderTableCell clockedInAtCell}
            {renderTableCell clockedOutAtCell}
            {renderTableCell lunchDurationCell}
            {renderTableCell workDoneCell}
            {renderTableCell invoiceTranslationCell}
            {renderTableCell hoursWorkedCell}
        </tr>
    |]

renderTableCell :: TableCell -> Html
renderTableCell tableCell =
    case tableCell of
        ShowCell {..} ->
            [hsx|
                <td scope="col" class={editableFieldToClass editableField}>
                    <a href={editAction} class="cell-value">{nl2br value}</a>
                </td>
            |]
        EditCell {..} ->
            [hsx|
                <td class={editableFieldToClass editableField}>
                    <form method="POST" action={saveAction} class="edit-form" data-disable-javascript-submission="false">
                        <div class="form-group">
                            {renderCellInput editableField value}
                        </div>
                        
                        <button class="btn btn-primary btn btn-primary btn-sm">Save</button>
                        <a href={cancelAction} class="btn btn-secondary btn-sm ml-2">Cancel</a>
                    </form>
                </td>
            |]

renderCellInput :: EditableField -> Text -> Html
renderCellInput editableField value =
    case editableField of
        WorkDoneField ->
            [hsx|
                <textarea type="text" name={editableFieldParam} placeholder="" class={editableFieldClasses <> " work-done-input"} value={value}>
                    {value}
                </textarea> 
            |]
        InvoiceTranslationField ->
            [hsx|
                <textarea type="text" name={editableFieldParam} placeholder="" class={editableFieldClasses <> " invoice-translation-input"} value={value}>
                    {value}
                </textarea> 
            |]
        ClockedInAtField ->
            [hsx|
                <input type="text" name={editableFieldParam} placeholder="" class={editableTimeFieldClasses} value={value}>
            |]
        ClockedOutAtField ->
            [hsx|
                <input type="text" name={editableFieldParam} placeholder="" class={editableTimeFieldClasses} value={value}>
            |]
        _ ->
            [hsx|
                <input type="text" name={editableFieldParam} placeholder="" class={editableFieldClasses} value={value}>
            |]
  where
    editableFieldParam = editableFieldToParam editableField
    editableFieldClasses = editableFieldToClass editableField <> "-input editable-cell form-control"
    editableTimeFieldClasses = editableFieldToClass editableField <> "-input editable-cell flatpickr-time-input form-control"

renderTotalHoursRow :: TotalHoursRow -> Html
renderTotalHoursRow TotalHoursRow {..} =
    [hsx|
        <tr class="table-active">
            <th scope="row">Total Hours</th>
            <td></td>
            <td></td>
            <td></td>
            <td></td>
            <td></td>
            <td></td>
            <td></td>
            <td>{totalHours}</td>
        </tr>
    |]

renderColumnNavigation :: ColumnNavigation -> Html
renderColumnNavigation ColumnNavigation {..} =
    [hsx|
        <ul class="column-nav m-0 p-0 mb-2 d-flex d-lg-none">
            <li class="column-nav-item flex-even d-flex justify-content-center">
                <a class={"column-nav-link text-center " <> peopleLinkClass} href={peopleAction}>People</a>
            </li>
            <li class="column-nav-item flex-even d-flex justify-content-center">
                <a class={"column-nav-link text-center " <> timecardsLinkClass} href={timecardsAction}>Timecards</a>
            </li>
        </ul>
    |]

columnToParam :: Column -> Text
columnToParam PeopleColumn = "people"
columnToParam TimecardsColumn = "timecards"

editableFieldToParam :: EditableField -> Text
editableFieldToParam JobNameField = "jobName"
editableFieldToParam ClockedInAtField = "clockedInAt"
editableFieldToParam ClockedOutAtField = "clockedOutAt"
editableFieldToParam LunchDurationField = "lunchDuration"
editableFieldToParam WorkDoneField = "workDone"
editableFieldToParam InvoiceTranslationField = "invoiceTranslation"
editableFieldToParam HoursWorkedField = "hoursWorked"

editableFieldToClass :: EditableField -> Text
editableFieldToClass JobNameField = "job-name"
editableFieldToClass ClockedInAtField = "clocked-in-at"
editableFieldToClass ClockedOutAtField = "clocked-out-at"
editableFieldToClass LunchDurationField = "lunch-duration"
editableFieldToClass WorkDoneField = "work-done"
editableFieldToClass InvoiceTranslationField = "invoice-translation"
editableFieldToClass HoursWorkedField = "hours-worked"

joinWithLineBreaks :: Text -> Html5.Html
joinWithLineBreaks value = value |> atLeastOneLine |> toHtml |> joinHtml
  where
    atLeastOneLine value =
        case lines value of
            [] -> [""]
            lines -> lines
    toHtml = map (\line -> [hsx|{line}|])
    joinHtml = foldl1' (\html line -> [hsx|{html}<br/>{line}|])

styles :: Html
styles =
    [hsx|
        <style>
            @media only screen and (min-width: 992px) {
                :root {
                    --column-nav-height: 0rem;
                    --mobile-browser-bar-height: 0rem;
                }
            }

            @media only screen and (max-width: 992px) {
                :root {
                    --column-nav-height: 3rem;
                    --mobile-browser-bar-height: 8rem;
                }
            }

            :root {
                --section-nav-height: 7.25rem;
                --total-nav-height: calc(var(--section-nav-height) + var(--column-nav-height) + var(--mobile-browser-bar-height));
                --screen-height: 100vh;
                --people-list-min-width: 18.75rem;
            }

            .column-nav {
                height: var(--column-nav-height);
            }

            .column-nav-item {
                list-style-type: none;
            }

            .column-nav-link {
                width: 100%;
                line-height: 2.5rem;
            }

            .people-list {
                height: calc(var(--screen-height) - var(--total-nav-height));
                min-width: var(--people-list-min-width);
                overflow-y: scroll;
            }

            .timecards-column {
                height: calc(var(--screen-height) - var(--total-nav-height));
                overflow-y: scroll;
                font-size: .9rem;
            }

            @media only screen and (min-width: 992px) {
                .timecards-column {
                    max-width: calc(100vw - calc(var(--people-list-min-width) + 2rem));
                    overflow-x: hidden;
                }

                .day-of-week {
                    width: 7rem;
                }

                .job-name {
                    width: 10rem;
                }

                .clocked-in-at {
                    width: 8rem;
                }

                .clocked-out-at {
                    width: 8rem;
                }

                .lunch-duration {
                    width: 8rem;
                }

                .work-done {
                    width: 14rem;
                }

                .invoice-translation {
                    width: 14rem;
                }

                .hours-worked {
                    width: 5rem;
                }
            }

            @media only screen and (max-width: 992px) {
                .timecards-column {
                    max-width: 100vw;
                }

                .day-of-week {
                    min-width: 7rem;
                }

                .job-name {
                    min-width: 10rem;
                }

                .clocked-in-at {
                    min-width: 8rem;
                }

                .clocked-out-at {
                    min-width: 8rem;
                }

                .lunch-duration {
                    min-width: 8rem;
                }

                .work-done {
                    min-width: 14rem;
                }

                .invoice-translation {
                    min-width: 14rem;
                }

                .hours-worked {
                    min-width: 5rem;
                }
            }

            .cell-value:empty:before {
                content: "--";
            }

            .cell-value {
                color: inherit;
            }

            .work-done-input {
                min-height: 25rem;
            }

            .invoice-translation-input {
                min-height: 25rem;
            }

            .editable-cell {
                font-size: .9rem;
                min-width: 8rem;
            }

            .sticky-header thead th { 
                position: sticky;
                top: 0;
                z-index: 1;
                background: white;
                border: none;
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
