module Web.View.Timecards.Index where

import Web.View.Navigation (Section (Timecards), renderNavigation)
import Web.View.Prelude

import IHP.View.TimeAgo as TO
import Web.View.Service.Style (removeScrollbars)
import Web.View.Service.Time (weekday)

data IndexView = IndexView
    { people :: ![Person]
    , personSelection :: !PersonSelection
    }

data PersonSelection
    = NoPersonSelected
    | PersonSelected
        { selectedPerson :: !Person
        , timecards :: ![Timecard]
        }

newtype Timecard = Timecard
    { timecardEntries :: [TimecardEntry]
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
                <div class="timecard-column col-8">
                    {forEach timecards (renderTimecard selectedPerson)}
                </div>
            |]

renderTimecard :: Person -> Timecard -> Html
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
                        {forEach (get #timecardEntries timecard) renderTimecardRow}
                        {renderLastRow $ totalHoursWorked timecard}
                    </tbody>
                </table>
            </div>
        </div>
    |]

renderTimecardRow :: TimecardEntry -> Html
renderTimecardRow timecardEntry =
    [hsx|
        <tr>
            <th scope="row">{weekday'}</th>
            <td>{date}</td>
            <td>{get #jobName timecardEntry}</td>
            <td class="work-done">{get #workDone timecardEntry}</td>
            <td>{get #hoursWorked timecardEntry}</td>
        </tr>
    |]
  where
    weekday' = weekday (get #date timecardEntry)
    date = TO.date (get #date timecardEntry)

renderLastRow :: Double -> Html
renderLastRow hours =
    [hsx|
        <tr class="table-active">
            <th scope="row" colspan="4">Total Hours</th>
            <td>{hours}</td>
        </tr>
    |]

dateRange :: Timecard -> Html
dateRange Timecard {..} =
    case (head timecardEntries, last timecardEntries) of
        (Just firstEntry, Just lastEntry) ->
            [hsx|
                <span>
                    {TO.date (get #date firstEntry)} - {TO.date (get #date lastEntry)}
                </span>
            |]
        _ -> [hsx||]

totalHoursWorked :: Timecard -> Double
totalHoursWorked Timecard {..} =
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
        }

        .work-done {
            width: 500px;
        }
    </style>

    {removeScrollbars}
|]
