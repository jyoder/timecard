module Web.View.AuditEntries.Index where

import qualified Application.Audit.Query as Audit.Query
import qualified Application.People.View as V
import Web.View.Navigation.People (BadgeVisibility (..), PeopleNavigation, buildPeopleNavigation, renderPeopleNavigation)
import Web.View.Navigation.Section (Section (AuditEntries), renderSectionNavigation)
import Web.View.Prelude hiding (Page)
import Web.View.Service.Style (removeScrollbars)
import Web.View.Service.Time (formatDateTime)

data IndexView = IndexView
    { people :: ![V.Person]
    , personSelection :: !PersonSelection
    , currentColumn :: !Column
    }

data PersonSelection
    = NoPersonSelected
    | PersonSelected
        { selectedPerson :: !Person
        , auditEntries :: ![Audit.Query.Row]
        }

data Page = Page
    { selectedPerson :: !(Maybe Person)
    , peopleNavigationClasses :: !Text
    , peopleNavigation :: !(PeopleNavigation AuditEntriesController)
    , auditColumnClasses :: !Text
    , auditColumn :: !AuditColumn
    , columnNavigation :: !ColumnNavigation
    }
    deriving (Eq, Show)

data AuditColumn
    = AuditColumnNotVisible
    | AuditColumnVisible
        { auditEntriesTable :: !AuditEntriesTable
        }
    deriving (Eq, Show)

newtype AuditEntriesTable = AuditEntriesTable
    { auditEntryRows :: [AuditEntryRow]
    }
    deriving (Eq, Show)

data AuditEntryRow = AuditEntryRow
    { createdAt :: !Text
    , createdBy :: !Text
    , action :: !Text
    , actionContext :: !Text
    }
    deriving (Eq, Show)

data ColumnNavigation = ColumnNavigation
    { peopleLinkClass :: !Text
    , peopleAction :: !AuditEntriesController
    , auditLinkClass :: !Text
    , auditAction :: !AuditEntriesController
    }
    deriving (Eq, Show)

data Column
    = PeopleColumn
    | EntriesColumn
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
                        AuditEntriesPersonSelectionAction
                            { selectedPersonId
                            , column = Just $ columnToParam EntriesColumn
                            }
                    )
                    selectedPerson
                    people
            , auditColumnClasses = columnClasses EntriesColumn currentColumn
            , auditColumn = buildAuditColumn view
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

buildAuditColumn :: IndexView -> AuditColumn
buildAuditColumn IndexView {..} =
    case personSelection of
        NoPersonSelected -> AuditColumnNotVisible
        PersonSelected {..} ->
            case auditEntries of
                [] -> AuditColumnNotVisible
                auditEntries ->
                    AuditColumnVisible $
                        AuditEntriesTable
                            { auditEntryRows = buildAuditEntryRow <$> auditEntries
                            }

buildAuditEntryRow :: Audit.Query.Row -> AuditEntryRow
buildAuditEntryRow Audit.Query.Row {..} =
    AuditEntryRow
        { createdAt = formatDateTime createdAt
        , createdBy = createdBy' createdBy
        , action = show action
        , actionContext = actionContext
        }
  where
    createdBy' email = fromMaybe "System" email

buildColumnNavigation :: PersonSelection -> Column -> ColumnNavigation
buildColumnNavigation personSelection currentColumn =
    ColumnNavigation
        { peopleLinkClass = linkClass PeopleColumn
        , peopleAction = action PeopleColumn
        , auditLinkClass = linkClass EntriesColumn
        , auditAction = action EntriesColumn
        }
  where
    linkClass column = if column == currentColumn then "text-dark" else "text-muted"
    action column = case personSelection of
        NoPersonSelected -> AuditEntriesAction
        PersonSelected {..} ->
            AuditEntriesPersonSelectionAction
                { selectedPersonId = get #id selectedPerson
                , column = Just $ columnToParam column
                }

columnToParam :: Column -> Text
columnToParam PeopleColumn = "people"
columnToParam EntriesColumn = "entries"

renderPage :: Page -> Html
renderPage Page {..} =
    [hsx|
        <div class="d-flex flex-column">
            {renderSectionNavigation AuditEntries selectedPerson}
            {renderColumnNavigation columnNavigation}
            
            <div class="d-flex flex-row">
                <div class={"mr-lg-3 flex-column " <> peopleNavigationClasses}>
                    {renderPeopleNavigation peopleNavigation}
                </div>
                <div class={"flex-column " <> auditColumnClasses}>
                    {renderAuditColumn auditColumn}
                </div>
            </div>
        </div>

        {styles}
    |]

renderAuditColumn :: AuditColumn -> Html
renderAuditColumn auditColumn =
    case auditColumn of
        AuditColumnNotVisible ->
            [hsx||]
        AuditColumnVisible {..} ->
            [hsx|
                <div class="audit-column mr-3 flex-grow-1">
                    <div class="d-none d-lg-block"></div>
                    {renderAuditEntriesTable auditEntriesTable}
                </div>
            |]

renderAuditEntriesTable :: AuditEntriesTable -> Html
renderAuditEntriesTable AuditEntriesTable {..} =
    [hsx|
        <table class="table table-striped sticky-header">
            <thead>
                <tr>
                    <th scope="col">Occurred At</th>
                    <th scope="col">Performed By</th>
                    <th scope="col">Action</th>
                    <th scope="col" class="d-none d-md-table-cell">Context</th>
                </tr>
            </thead>
            <tbody>
                {forEach auditEntryRows renderAuditEntryRow}
            </tbody>
        </table>
    |]

renderAuditEntryRow :: AuditEntryRow -> Html
renderAuditEntryRow AuditEntryRow {..} =
    [hsx|
        <tr>
            <td class="occurred-at"><time class="date-time" datetime={createdAt}>{createdAt}</time></td>
            <td class="performed-by">{createdBy}</td>
            <td>{action}</td>
            <td class="d-none d-md-table-cell"><pre><code class="audit-context">{nl2br actionContext}</code></pre></td>
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
                <a class={"column-nav-link text-center " <> auditLinkClass} href={auditAction}>Entries</a>
            </li>
        </ul>
    |]

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
                min-width: 18.75rem;
                overflow-y: scroll;
            }

            .audit-column {
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

            .occurred-at {
                width: 12rem;
            }

            .performed-by {
                width: 12rem;
            }

            .audit-context {
                white-space: pre-wrap;
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