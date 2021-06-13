module Web.View.Timecards.ShowPdf where

import qualified Application.Timecard.Timecard as Timecard
import Web.View.Prelude
import Web.View.Service.Time (formatDay)

data ShowPdfView = ShowPdfView
    { selectedPerson :: !Person
    , timecard :: !Timecard.T
    }

instance View ShowPdfView where
    beforeRender view = do
        setLayout (\view -> view)

    html view =
        [hsx|
            <div class="content container-fluid">
                <div class="row align-items start">
                    {renderTimecardColumn view}
                </div>
            </div>
        |]

renderTimecardColumn :: ShowPdfView -> Html
renderTimecardColumn ShowPdfView {..} =
    [hsx|
        <div class="timecard-column col-12">
            {renderTimecard selectedPerson timecard}
        </div>
    |]

renderTimecard :: Person -> Timecard.T -> Html
renderTimecard selectedPerson timecard =
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
                            <th scope="col">Hours</th>
                        </tr>
                    </thead>
                    <tbody>
                        {forEach (get #timecardEntries timecard) (renderTimecardRow selectedPerson)}
                        {renderLastRow $ totalHoursWorked timecard}
                    </tbody>
                </table>
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

renderTimecardRow :: Person -> TimecardEntry -> Html
renderTimecardRow personActivity timecardEntry =
    [hsx|
        <tr>
            <th scope="row">{dayOfWeek'}</th>
            <td>{date}</td>
            <td>{get #jobName timecardEntry}</td>
            <td class="work-done">{get #workDone timecardEntry}</td>
            <td>{get #hoursWorked timecardEntry}</td>
        </tr>
    |]
  where
    dayOfWeek' = dayOfWeek $ get #date timecardEntry
    date = formatDay $ get #date timecardEntry

renderLastRow :: Double -> Html
renderLastRow hours =
    [hsx|
        <tr class="table-secondary">
            <th scope="row">Total Hours</th>
            <td></td>
            <td></td>
            <td></td>
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
            .work-done {
                width: 300px;
            }
        </style>
    |]