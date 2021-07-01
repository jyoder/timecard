module Web.View.Timecards.Index where

import qualified Application.Timecard.Queries as Q
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
        , timecards :: ![Q.Timecard]
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
            {renderNavigation Timecards selectedPerson'}

            <div class="row align-items start">
                {renderPeopleColumn view}
                {renderTimecardColumn view}
            </div>

            {styles}
        |]
      where
        selectedPerson' = case get #personSelection view of
            NoPersonSelected -> Nothing
            PersonSelected {..} -> Just selectedPerson

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

renderTimecard :: Person -> PersonActivity -> Q.Timecard -> Html
renderTimecard selectedPerson personActivity timecard =
    [hsx|
        <div class="card mb-5">
            <div class="card-header d-flex justify-content-start mb-2">
                <h5>Week Of {formatDay weekOf}</h5>
                <div class="ml-2">
                    {renderTimecardStatus timecard}
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
                            <th scope="col">Work Done</th>
                            <th scope="col">Invoice Translation</th>
                            <th scope="col">Hours</th>
                        </tr>
                    </thead>
                    <tbody>
                        {forEach (get #entries timecard) (renderTimecardRow selectedPerson personActivity)}
                        {renderLastRow $ totalHoursWorked timecard}
                    </tbody>
                </table>
                <a href={downloadAction} download={downloadFilename}>Download PDF</a>
            </div>
        </div>
    |]
  where
    lastName = get #lastName selectedPerson
    firstName = get #firstName selectedPerson
    downloadAction = TimecardDownloadTimecardAction $ get #id timecard
    downloadFilename = showWeekOf <> "-" <> lastName <> "-" <> firstName <> ".pdf"
    showWeekOf = show $ get #weekOf timecard
    weekOf = get #weekOf timecard

renderTimecardStatus :: Q.Timecard -> Html
renderTimecardStatus timecard =
    case get #status timecard of
        Q.TimecardInProgress ->
            [hsx|
                <span class="badge badge-pill badge-secondary">In Progress</span>
            |]
        Q.TimecardReadyForReview ->
            [hsx|
                <span class="badge badge-pill badge-primary">Ready for Review</span>
            |]
        Q.TimecardUnderReview _ ->
            [hsx|
                <span class="badge badge-pill badge-primary">Under Review</span>
            |]
        Q.TimecardSigned _ ->
            [hsx|
                <span class="badge badge-pill badge-success">Signed</span>
            |]

renderTimecardRow :: Person -> PersonActivity -> Q.TimecardEntry -> Html
renderTimecardRow selectedPerson personActivity timecardEntry =
    [hsx|
        <tr>
            <th scope="row">{dayOfWeek'}</th>
            <td>{date}</td>
            <td>{get #jobName timecardEntry}</td>
            <td class="work-done">{get #workDone timecardEntry}</td>
            {renderInvoiceTranslation selectedPerson personActivity timecardEntry}
            <td>{get #hoursWorked timecardEntry}</td>
        </tr>
    |]
  where
    dayOfWeek' = dayOfWeek $ get #date timecardEntry
    date = formatDay $ get #date timecardEntry

renderInvoiceTranslation :: Person -> PersonActivity -> Q.TimecardEntry -> Html
renderInvoiceTranslation selectedPerson personActivity timecardEntry =
    case personActivity of
        Viewing -> renderViewInvoiceTranslation timecardEntry
        Editing {..} ->
            if get #id timecardEntry == get #id selectedTimecardEntry
                then
                    [hsx|
                        <td class="invoice-translation">
                            {renderInvoiceTranslationForm selectedPerson selectedTimecardEntry}
                        </td>
                    |]
                else renderViewInvoiceTranslation timecardEntry

renderViewInvoiceTranslation :: Q.TimecardEntry -> Html
renderViewInvoiceTranslation timecardEntry =
    [hsx|
        <td class="invoice-translation">
            {get #invoiceTranslation timecardEntry} (<a href={editAction}>Edit</a>)
        </td>
    |]
  where
    editAction = TimecardEditTimecardEntryAction (get #id timecardEntry)

renderInvoiceTranslationForm :: Person -> TimecardEntry -> Html
renderInvoiceTranslationForm selectedPerson timecardEntry =
    formForWithOptions
        timecardEntry
        formOptions
        [hsx| 
            {(textareaField #invoiceTranslation) {disableLabel = True}}

            {hiddenField #id}
            
            {submitButton { label = "Save", buttonClass = "btn btn-primary btn-sm"}}
            <a href={personSelectionAction} class="btn btn-secondary btn-sm ml-2">Cancel</a>
        |]
  where
    formOptions formContext =
        formContext
            |> set #formId "edit-timecard-entry-form"
            |> set #formAction (pathTo TimecardUpdateTimecardEntryAction)
    personSelectionAction = TimecardPersonSelectionAction (get #id selectedPerson)

renderLastRow :: Double -> Html
renderLastRow hours =
    [hsx|
        <tr class="table-active">
            <th scope="row" colspan="5">Total Hours</th>
            <td>{hours}</td>
        </tr>
    |]

totalHoursWorked :: Q.Timecard -> Double
totalHoursWorked Q.Timecard {..} =
    sum (get #hoursWorked <$> entries)

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
