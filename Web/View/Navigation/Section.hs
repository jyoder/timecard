module Web.View.Navigation.Section (Section (..), renderSectionNavigation) where

import IHP.RouterSupport (HasPath)
import Web.View.Prelude

data Section
    = Timecards
    | Communications
    deriving (Eq)

renderSectionNavigation :: Section -> Maybe Person -> Html
renderSectionNavigation currentSection selectedPerson =
    [hsx|
        <nav class="navbar navbar-expand navbar-light bg-light m-0 mb-3">
            <div class="container-fluid">
                <span class="navbar-brand mb-0 h1 d-none d-lg-inline" href="#">Constructable</span>
                <div class="collapse navbar-collapse">
                    <ul class="navbar-nav mb-0">
                        {renderItem timecardsAction Timecards "Timecards" currentSection}
                        {renderItem communicationsAction Communications "Communications" currentSection}
                    </ul>
                </div>
                <a 
                    href={DeleteSessionAction}
                    class="btn btn-outline-primary js-delete js-delete-no-confirm">
                    Logout
                </a>
            </div>
        </nav>
    |]
  where
    timecardsAction = case selectedPerson of
        Just selectedPerson -> TimecardPersonSelectionAction (get #id selectedPerson)
        Nothing -> TimecardsAction
    communicationsAction = case selectedPerson of
        Just selectedPerson ->
            CommunicationsPersonSelectionAction
                { selectedPersonId = get #id selectedPerson
                , column = Nothing
                }
        Nothing -> CommunicationsAction

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
