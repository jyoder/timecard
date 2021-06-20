module Web.View.Timecards.ShowPdf where

import qualified Application.Timecard.TimecardQueries as Q
import Web.View.Prelude
import Web.View.Service.Time (formatDay)
import qualified Prelude as P

data ShowPdfView = ShowPdfView
    { selectedPerson :: !Person
    , timecard :: !Q.Timecard
    }

instance View ShowPdfView where
    beforeRender view = setLayout P.id

    html ShowPdfView {..} =
        [hsx|
            <div class="content container-fluid">
                <div class="col-12">
                    {renderTimecard selectedPerson timecard}
                </div>
            </div>
            {styles}
        |]

renderTimecard :: Person -> Q.Timecard -> Html
renderTimecard selectedPerson timecard =
    [hsx|
        <div class="card mb-5">
            <h5 class="card-header">
                Week Of {formatDay $ get #weekOf timecard}
            </h5>
            
            <div class="card-body">
                <h5 class="card-title">
                    {lastName}, {firstName}
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
                        {forEach (get #entries timecard) (renderTimecardRow selectedPerson)}
                        {renderLastRow $ totalHoursWorked timecard}
                    </tbody>
                </table>
            </div>
        </div>
    |]
  where
    lastName = get #lastName selectedPerson
    firstName = get #firstName selectedPerson

renderTimecardRow :: Person -> Q.TimecardEntry -> Html
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

totalHoursWorked :: Q.Timecard -> Double
totalHoursWorked Q.Timecard {..} =
    sum (get #hoursWorked <$> entries)

styles :: Html
styles =
    [hsx|
        <style>
            .work-done {
                width: 300px;
            }
        </style>
    |]
