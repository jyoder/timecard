module Web.View.Navigation.Section (Section (..), renderSectionNavigation) where

import IHP.RouterSupport (HasPath)
import Web.View.Prelude

data Section
    = Timecards
    | Communications
    | AuditEntries
    | Reports
    deriving (Eq)

renderSectionNavigation :: Section -> Maybe Person -> Html
renderSectionNavigation currentSection selectedPerson =
    [hsx|
        <nav class="navbar navbar-expand navbar-light bg-light p-0 pl-3 m-0 mb-3">
            <div class="container-fluid">
                <div class="navbar-brand h1 m-0 pb-4 pr-3 d-none d-lg-inline" href="#">Constructable</div>
                <div class="collapse navbar-collapse m-0" style="overflow-x: scroll;">
                    <ul class="navbar-nav">
                        {renderItem timecardsAction Timecards "Timecards" currentSection}
                        {renderItem communicationsAction Communications "Communications" currentSection}
                        {renderItem auditEntriesAction AuditEntries "Audit" currentSection}
                        {renderItem ReportsAction Reports "Reports" currentSection}
                    </ul>
                </div>
                <a href={DeleteSessionAction} class="btn btn-outline-primary js-delete js-delete-no-confirm ml-3 mr-3 mb-3">
                    Logout
                </a>
            </div>
        </nav>
    |]
  where
    timecardsAction = case selectedPerson of
        Just selectedPerson ->
            TimecardPersonSelectionAction
                { selectedPersonId = get #id selectedPerson
                , column = Nothing
                , jumpToTop = Just 1
                }
        Nothing -> TimecardsAction
    communicationsAction = case selectedPerson of
        Just selectedPerson ->
            CommunicationsPersonSelectionAction
                { selectedPersonId = get #id selectedPerson
                , column = Nothing
                }
        Nothing -> CommunicationsAction
    auditEntriesAction = case selectedPerson of
        Just selectedPerson ->
            AuditEntriesPersonSelectionAction
                { selectedPersonId = get #id selectedPerson
                , column = Nothing
                }
        Nothing -> AuditEntriesAction
    ReportsAction = ReportsAction

renderItem :: (HasPath action) => action -> Section -> Text -> Section -> Html
renderItem action newSection label currentSection =
    [hsx|
        <li class="nav-item">
            <a 
                class={"nav-link " <> activeClass currentSection newSection}
                aria-current={ariaCurrent currentSection newSection}
                href={pathTo action}>
                {label}
            </a>
        </li>
    |]

activeClass :: Section -> Section -> Text
activeClass newSection currentSection = if newSection == currentSection then "active" else ""

ariaCurrent :: Section -> Section -> Text
ariaCurrent newSection currentSection = if newSection == currentSection then "page" else ""
