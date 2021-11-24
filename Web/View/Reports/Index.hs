module Web.View.Reports.Index where

import qualified Application.Reports.AutomationQuery as Reports.AutomationQuery
import Data.Time.Calendar.WeekDate (toWeekDate)
import Web.View.Navigation.Section (Section (Reports), renderSectionNavigation)
import Web.View.Prelude hiding (Page, Table)
import Web.View.Service.Style (removeScrollbars)
import Web.View.Service.Time (formatDayNoYear)

newtype IndexView = IndexView
    { dailyReportRows :: [Reports.AutomationQuery.Row]
    }

newtype Page = Page
    { dailyReportTable :: Table
    }
    deriving (Eq, Show)

newtype Table = Table
    { rows :: [Row]
    }
    deriving (Show, Eq)

data Row = Row
    { name :: !Text
    , automationRate :: !Text
    , cells :: ![Cell]
    }
    deriving (Show, Eq)

data Cell = Cell
    { date :: !Text
    , classes :: !Text
    }
    deriving (Show, Eq)

instance View IndexView where
    html view = buildPage view |> renderPage

buildPage :: IndexView -> Page
buildPage IndexView {..} = Page {dailyReportTable = buildTable dailyReportRows}

buildTable :: [Reports.AutomationQuery.Row] -> Table
buildTable rows = Table {rows = rows |> groupByPerson |> buildRows}

groupByPerson :: [Reports.AutomationQuery.Row] -> [[Reports.AutomationQuery.Row]]
groupByPerson = groupBy (\r1 r2 -> get #personId r1 == get #personId r2)

buildRows :: [[Reports.AutomationQuery.Row]] -> [Row]
buildRows rowGroups = buildRow <$> rowGroups

buildRow :: [Reports.AutomationQuery.Row] -> Row
buildRow rowGroup =
    Row
        { name
        , automationRate = formattedAutomationRate
        , cells = buildCells rowGroup
        }
  where
    name = formatName rowGroup
    formattedAutomationRate = show (round (automationRate * 100)) <> "%"
    automationRate = if activeRows > 0 then fromIntegral fullyAutomatedRows / fromIntegral activeRows else 0.0
    fullyAutomatedRows = length $ filter (\row -> get #automationStatus row == Reports.AutomationQuery.FullyAutomated) rowGroup
    activeRows = length $ filter (\row -> get #automationStatus row /= Reports.AutomationQuery.NoActivity) rowGroup

formatName :: [Reports.AutomationQuery.Row] -> Text
formatName rows =
    case rows of
        row : _ -> get #personLastName row <> ", " <> get #personFirstName row
        [] -> "Unknown Person"

buildCells :: [Reports.AutomationQuery.Row] -> [Cell]
buildCells rowGroup = buildCell <$> rowGroup

buildCell :: Reports.AutomationQuery.Row -> Cell
buildCell Reports.AutomationQuery.Row {..} =
    Cell
        { date = formatDateIfMonday date
        , classes = automationStatusClass automationStatus
        }

formatDateIfMonday :: Day -> Text
formatDateIfMonday date =
    case toWeekDate date of
        (_, _, 1) -> formatDayNoYear date
        _ -> ""

automationStatusClass :: Reports.AutomationQuery.AutomationStatus -> Text
automationStatusClass Reports.AutomationQuery.FullyAutomated = "fully-automated"
automationStatusClass Reports.AutomationQuery.NotFullyAutomated = "not-fully-automated"
automationStatusClass Reports.AutomationQuery.NoActivity = "no-activity"

renderPage :: Page -> Html
renderPage Page {..} =
    [hsx|
        <div class="d-flex flex-column">
            {renderSectionNavigation Reports Nothing}
        </div>

        <div class="ml-lg-2 mr-lg-2">
            <h1 class="mb-4">Automation Rates</h1>

            {renderTable dailyReportTable}

            <div class="table-responsive mt-4">
                <table class="table table-sm table-bordered legend">
                    <tbody>
                        <tr>
                            <th scope="row"><div class="cell fully-automated"></div></th>
                            <td>Fully automated</td>
                        </tr>

                        <tr>
                            <th scope="row"><div class="cell not-fully-automated"></div></th>
                            <td>Not fully automated</td>
                        </tr>
                        <tr>
                            <th scope="row"><div class="cell no-activity"></div></th>
                            <td>No activity</td>
                        </tr>
                    </tbody>
                </table>
            </div>
        </div>

        {styles}
    |]

renderTable :: Table -> Html
renderTable Table {..} =
    [hsx|
        <div class="table-responsive">
            <table class="table table-sm">
                <thead>
                    {renderHeaderRow rows}
                </thead>
                <tbody>
                    {forEach rows renderRow}
                </tbody>
            </table>
        </div>
    |]

renderHeaderRow :: [Row] -> Html
renderHeaderRow rows =
    case rows of
        row : _ ->
            [hsx|
                <tr> 
                    <th class="col header-crew-member">Crew Member</th>
                    <th class="col header-automation-rate">Automation Rate</th>
                    {forEach (get #cells row) renderHeaderCell}
                </tr> 
            |]
        [] ->
            [hsx||]

renderHeaderCell :: Cell -> Html
renderHeaderCell Cell {..} =
    [hsx|
        <th class="col header-date">{date}</th>
    |]

renderRow :: Row -> Html
renderRow Row {..} =
    [hsx|
        <tr>
            <th scope="row">{name}</th>
            <td>{automationRate}</td>
            {forEach cells renderCell}
        </tr>
    |]

renderCell :: Cell -> Html
renderCell Cell {..} =
    [hsx|
        <td>
            <div class={"cell " <> classes}></div>
        </td>
    |]

styles :: Html
styles =
    [hsx|
        <style>
            table {
                font-size: .9rem;
            }

            .header-crew-member {
                min-width: 12rem;
            }

            .header-automation-rate {
                min-width: 10rem;
            }

            .header-date {
                font-size: .6rem;
                min-width: 3rem;
            }

            .cell {
                border-radius: 50%;
                min-width: 1rem;
                max-width: 1rem;
                min-height: 1rem;
                max-height: 1rem;
            }

            .fully-automated {
                background-color: #6ee7b7;
            }

            .not-fully-automated {
                background-color: #fca5a5;
            }

            .no-activity {
                background-color: #d1d5db;
            }

            .legend {
                width: 25rem;
            }
        </style>

        {removeScrollbars}
    |]