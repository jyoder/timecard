module Web.View.Timecards.Index where

import qualified Application.Timecard.Timecard as Timecard
import IHP.View.TimeAgo as TO
import Web.View.Navigation (Section (Timecards), renderNavigation)
import Web.View.Prelude
import Web.View.Service.Style (removeScrollbars)
import Web.View.Service.Time (formatDay)

data IndexView = IndexView
    { people :: ![Person]
    , personSelection :: !PersonSelection
    }

data PersonSelection
    = NoPersonSelected
    | PersonSelected
        { selectedPerson :: !Person
        , timecards :: ![Timecard.T]
        , personActivity :: !PersonActivity
        }

data PersonActivity
    = Viewing
    | Editing
        { selectedTimecardEntry :: !TimecardEntry
        }

instance View IndexView where
    html view =
        [hsx|
            {renderNavigation Timecards}

            <div class="row align-items start">
                {renderPeopleColumn view}
                {renderTimecardColumn view}
            </div>

            {styles}
        |]

renderPeopleColumn :: IndexView -> Html
renderPeopleColumn IndexView {..} =
    [hsx|
        <div class="people-column col-2">
            <div class="list-group">
                {forEach people renderPerson'}
            </div>
        </div>        
    |]
  where
    renderPerson' = case personSelection of
        NoPersonSelected -> renderPerson False
        PersonSelected {..} ->
            let isSelected person = get #id person == get #id selectedPerson
             in (\person -> renderPerson (isSelected person) person)

renderPerson :: Bool -> Person -> Html
renderPerson isSelected person =
    [hsx|
        <a
            href={TimecardPersonSelectionAction (get #id person)}
            class={"list-group-item " <> activeClass}
            aria-current={ariaCurrent}>
            {get #firstName person} {get #lastName person}
        </a>
    |]
  where
    activeClass = if isSelected then "active" else "" :: Text
    ariaCurrent = if isSelected then "true" else "false" :: Text

renderTimecardColumn :: IndexView -> Html
renderTimecardColumn IndexView {..} =
    case personSelection of
        NoPersonSelected -> [hsx||]
        PersonSelected {..} ->
            [hsx|
                <div class="timecard-column col-10">
                    {forEach timecards (renderTimecard selectedPerson personActivity)}
                </div>
            |]

renderTimecard :: Person -> PersonActivity -> Timecard.T -> Html
renderTimecard selectedPerson personActivity timecard =
    [hsx|
        <div class="card mb-5">
            <h5 class="card-header">{dateRange timecard}</h5>
            
            <div class="card-body">
                <h5 class="card-title">
                    {get #lastName selectedPerson}, {get #firstName selectedPerson}
                </h5>

                <table class="table">
                    <thead>
                        <tr>
                            <th scope="col">Day</th>
                            <th scope="col">Date</th>
                            <th scope="col">Job</th>
                            <th scope="col">Work Done</th>
                            <th scope="col">Invoice Translation</th>
                            <th scope="col">Hours</th>
                        </tr>
                    </thead>
                    <tbody>
                        {forEach (get #timecardEntries timecard) (renderTimecardRow personActivity)}
                        {renderLastRow $ totalHoursWorked timecard}
                    </tbody>
                </table>
                <a href={downloadAction} download={downloadFilename}>Download PDF</a>
            </div>
        </div>
    |]
  where
    downloadAction = TimecardDownloadTimecardAction (get #id selectedPerson) weekOf
    downloadFilename = weekOf <> "-" <> lastName <> "-" <> firstName <> ".pdf"
    lastName = get #lastName selectedPerson
    firstName = get #firstName selectedPerson
    weekOf =
        let Timecard.T {..} = timecard
         in case head timecardEntries of
                Just timecardEntry -> show $ get #date timecardEntry
                Nothing -> ""

renderTimecardRow :: PersonActivity -> TimecardEntry -> Html
renderTimecardRow personActivity timecardEntry =
    [hsx|
        <tr>
            <th scope="row">{dayOfWeek'}</th>
            <td>{date}</td>
            <td>{get #jobName timecardEntry}</td>
            <td class="work-done">{get #workDone timecardEntry}</td>
            {renderInvoiceTranslation personActivity timecardEntry}
            <td>{get #hoursWorked timecardEntry}</td>
        </tr>
    |]
  where
    dayOfWeek' = dayOfWeek $ get #date timecardEntry
    date = formatDay $ get #date timecardEntry

renderInvoiceTranslation :: PersonActivity -> TimecardEntry -> Html
renderInvoiceTranslation personActivity timecardEntry =
    case personActivity of
        Viewing -> renderViewInvoiceTranslation timecardEntry
        Editing {..} ->
            if get #id timecardEntry == get #id selectedTimecardEntry
                then
                    [hsx|
                        <td class="invoice-translation">
                            {renderInvoiceTranslationForm selectedTimecardEntry}
                        </td>
                    |]
                else renderViewInvoiceTranslation timecardEntry

renderViewInvoiceTranslation :: TimecardEntry -> Html
renderViewInvoiceTranslation timecardEntry =
    [hsx|
        <td class="invoice-translation">
            {get #invoiceTranslation timecardEntry} (<a href={editAction}>Edit</a>)
        </td>
    |]
  where
    editAction = TimecardEditTimecardEntryAction (get #id timecardEntry)

renderInvoiceTranslationForm :: TimecardEntry -> Html
renderInvoiceTranslationForm timecardEntry =
    formForWithOptions
        timecardEntry
        formOptions
        [hsx| 
            {(textareaField #invoiceTranslation) {disableLabel = True}}
            
            {hiddenField #personId}
            {hiddenField #id}
            
            {submitButton { label = "Save", buttonClass = "btn btn-primary btn-sm"}}
            <a href={personSelectionAction} class="btn btn-secondary btn-sm ml-2">Cancel</a>
        |]
  where
    formOptions formContext =
        formContext
            |> set #formId "edit-timecard-entry-form"
            |> set #formAction (pathTo TimecardUpdateTimecardEntryAction)
    personSelectionAction = TimecardPersonSelectionAction (get #personId timecardEntry)

renderLastRow :: Double -> Html
renderLastRow hours =
    [hsx|
        <tr class="table-active">
            <th scope="row" colspan="5">Total Hours</th>
            <td>{hours}</td>
        </tr>
    |]

dateRange :: Timecard.T -> Html
dateRange Timecard.T {..} =
    case (head timecardEntries, last timecardEntries) of
        (Just firstEntry, Just lastEntry) ->
            [hsx|
                <span>
                    {formatDay $ get #date firstEntry} - {formatDay $ get #date lastEntry}
                </span>
            |]
        _ -> [hsx||]

totalHoursWorked :: Timecard.T -> Double
totalHoursWorked Timecard.T {..} =
    sum (get #hoursWorked <$> timecardEntries)

styles :: Html
styles =
    [hsx|
    <style>
        .people-column {
            height: calc(100vh - 150px);
            overflow-y: scroll;
        }

        .timecard-column {
            height: calc(100vh - 150px);
            overflow-y: scroll;
            font-size: .9rem;
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
