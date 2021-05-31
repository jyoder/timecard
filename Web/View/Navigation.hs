module Web.View.Navigation (Section (..), renderNavigation) where

import IHP.RouterSupport (HasPath)
import Web.View.Prelude

data Section
    = Communications
    | Timecards
    deriving (Eq)

renderNavigation :: Section -> Html
renderNavigation currentSection =
    [hsx|
        <nav class="navbar navbar-expand-lg navbar-light bg-light">
            <div class="container-fluid">
                <span class="navbar-brand mb-0 h1" href="#">Constructable</span>
                <div class="collapse navbar-collapse" id="navbarSupportedContent">
                    <ul class="navbar-nav me-auto mb-2 mb-lg-0">
                        {renderItem CommunicationsAction Communications "Communications" currentSection}
                        {renderItem TimecardsAction Timecards "Timecards" currentSection}
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
