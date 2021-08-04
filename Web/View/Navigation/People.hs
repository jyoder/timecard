module Web.View.Navigation.People (
    BadgeVisibility (..),
    PeopleNavigation (..),
    PersonItem (..),
    StateBadge (..),
    buildPeopleNavigation,
    buildPersonItem,
    buildStateBadge,
    personStateLabel,
    personStateClasses,
    renderPeopleNavigation,
) where

import qualified Application.People.View as V
import Generated.Types
import IHP.ControllerPrelude (Controller)
import IHP.Prelude
import IHP.RouterSupport (HasPath (..))
import Web.View.Prelude

data BadgeVisibility = BadgesVisible | BadgesHidden

newtype PeopleNavigation a = PeopleNavigation
    { personItems :: [PersonItem a]
    }
    deriving (Eq, Show)

data PersonItem a = PersonItem
    { selectionAction :: !a
    , activeClass :: !Text
    , ariaCurrent :: !Text
    , firstName :: !Text
    , lastName :: !Text
    , stateBadge :: !StateBadge
    }
    deriving (Eq, Show)

data StateBadge
    = VisibleBadge
        { label :: !Text
        , classes :: !Text
        }
    | HiddenBadge
    deriving (Eq, Show)

buildPeopleNavigation ::
    (HasPath a) =>
    BadgeVisibility ->
    (Id Person -> a) ->
    Maybe Person ->
    [V.Person] ->
    PeopleNavigation a
buildPeopleNavigation badgeVisiblity action selectedPerson people =
    PeopleNavigation
        { personItems = buildPersonItem' <$> people
        }
  where
    buildPersonItem' = case selectedPerson of
        Nothing -> buildPersonItem badgeVisiblity action False
        Just selectedPerson ->
            let isSelected person = get #id person == get #id selectedPerson
             in (\person -> buildPersonItem badgeVisiblity action (isSelected person) person)

buildPersonItem ::
    (HasPath a) =>
    BadgeVisibility ->
    (Id Person -> a) ->
    Bool ->
    V.Person ->
    PersonItem a
buildPersonItem badgeVisibility action isSelected person =
    PersonItem
        { selectionAction = action $ get #id person
        , activeClass = if isSelected then "active" else ""
        , ariaCurrent = if isSelected then "true" else "false"
        , firstName = get #firstName person
        , lastName = get #lastName person
        , stateBadge = buildStateBadge badgeVisibility person
        }

buildStateBadge :: BadgeVisibility -> V.Person -> StateBadge
buildStateBadge BadgesVisible person =
    VisibleBadge
        { label = personStateLabel $ get #state person
        , classes = personStateClasses $ get #state person
        }
buildStateBadge BadgesHidden _ =
    HiddenBadge

personStateLabel :: V.PersonState -> Text
personStateLabel V.PersonIdle = "Idle"
personStateLabel V.PersonAutoPilot = "Auto Pilot"
personStateLabel V.PersonNeedsAttention = "Needs Attention"

personStateClasses :: V.PersonState -> Text
personStateClasses V.PersonIdle = "badge badge-pill badge-light"
personStateClasses V.PersonAutoPilot = "badge badge-pill badge-light"
personStateClasses V.PersonNeedsAttention = "badge badge-pill badge-warning"

renderPeopleNavigation :: (HasPath a) => PeopleNavigation a -> Html
renderPeopleNavigation PeopleNavigation {..} =
    [hsx|
        <div class="people-column col-2">
            <div class="list-group">
                {forEach personItems renderItem}
            </div>
        </div>        
    |]

renderItem :: (HasPath a) => PersonItem a -> Html
renderItem PersonItem {..} =
    [hsx|
        <a
            href={pathTo selectionAction}
            class={"list-group-item " <> activeClass}
            aria-current={ariaCurrent}
        >
            <div class="d-flex justify-content-between align-items-center">
                <span>
                    {firstName} {lastName}
                </span>

                {renderStateBadge stateBadge}
            </div>
        </a>
    |]

renderStateBadge :: StateBadge -> Html
renderStateBadge VisibleBadge {..} =
    [hsx|
        <span class={classes}>
            {label}
        </span>
    |]
renderStateBadge HiddenBadge = [hsx||]
