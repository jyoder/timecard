module Web.View.Timecards.ShowPdf where

import qualified Application.Timecard.View as V
import Web.View.Prelude
import Web.View.Service.Time (formatDay)
import qualified Prelude as P

data ShowPdfView = ShowPdfView
    { selectedPerson :: !Person
    , timecard :: !V.Timecard
    }

data TimecardTable = TimecardTable
    { weekOf :: !Text
    , firstName :: !Text
    , lastName :: !Text
    , jobRows :: ![JobRow]
    , totalHoursRow :: !TotalHoursRow
    }
    deriving (Eq, Show)

data JobRow = JobRow
    { dayOfWeek' :: !Text
    , date :: !Text
    , jobName :: !Text
    , hoursWorked :: !Text
    , workDone :: !Text
    }
    deriving (Eq, Show)

newtype TotalHoursRow = TotalHoursRow
    { totalHours :: Text
    }
    deriving (Eq, Show)

instance View ShowPdfView where
    beforeRender view = setLayout P.id

    html ShowPdfView {..} =
        renderPage $ buildTimecardTable selectedPerson timecard

buildTimecardTable :: Person -> V.Timecard -> TimecardTable
buildTimecardTable selectedPerson timecard =
    TimecardTable
        { weekOf = formatDay $ get #weekOf timecard
        , firstName = get #firstName selectedPerson
        , lastName = get #lastName selectedPerson
        , jobRows = buildJobRow selectedPerson <$> get #entries timecard
        , totalHoursRow = buildTotalHoursRow $ get #entries timecard
        }

buildJobRow :: Person -> V.TimecardEntry -> JobRow
buildJobRow selectedPerson timecardEntry =
    JobRow
        { dayOfWeek' = show $ dayOfWeek $ get #date timecardEntry
        , date = formatDay $ get #date timecardEntry
        , jobName = get #jobName timecardEntry
        , hoursWorked = show $ get #hoursWorked timecardEntry
        , workDone = get #workDone timecardEntry
        }

buildTotalHoursRow :: [V.TimecardEntry] -> TotalHoursRow
buildTotalHoursRow timecardEntries =
    TotalHoursRow
        { totalHours = show $ sum $ get #hoursWorked <$> timecardEntries
        }

renderPage :: TimecardTable -> Html
renderPage timecardTable =
    [hsx|
        <div class="content container-fluid">
            <div class="col-12">
                {renderTimecardTable timecardTable}
            </div>
        </div>

        {styles}
    |]

renderTimecardTable :: TimecardTable -> Html
renderTimecardTable TimecardTable {..} =
    [hsx|
        <div class="card mb-5">
            <h5 class="card-header">
                Week Of {weekOf}
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
                        {forEach jobRows renderJobRow}
                        {renderTotalHoursRow totalHoursRow}
                    </tbody>
                </table>
            </div>
        </div>
    |]

renderJobRow :: JobRow -> Html
renderJobRow JobRow {..} =
    [hsx|
        <tr>
            <th scope="row">{dayOfWeek'}</th>
            <td>{date}</td>
            <td>{jobName}</td>
            <td class="work-done">{workDone}</td>
            <td>{hoursWorked}</td>
        </tr>
    |]

renderTotalHoursRow :: TotalHoursRow -> Html
renderTotalHoursRow TotalHoursRow {..} =
    [hsx|
        <tr class="table-secondary">
            <th scope="row">Total Hours</th>
            <td></td>
            <td></td>
            <td></td>
            <td>{totalHours}</td>
        </tr>
    |]

styles :: Html
styles =
    [hsx|
        <style>
            .work-done {
                width: 300px;
            }
        </style>
    |]
