module Web.View.Navigation.People where

import Generated.Types
import IHP.ControllerPrelude (Controller)
import IHP.Prelude
import IHP.RouterSupport (HasPath (..))
import Web.View.Prelude

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
    }
    deriving (Eq, Show)

buildPeopleNavigation ::
    (HasPath a) =>
    (Id Person -> a) ->
    Maybe Person ->
    [Person] ->
    PeopleNavigation a
buildPeopleNavigation action selectedPerson people =
    PeopleNavigation
        { personItems = buildPersonItem' <$> people
        }
  where
    buildPersonItem' = case selectedPerson of
        Nothing -> buildPersonItem action False
        Just selectedPerson ->
            let isSelected person = get #id person == get #id selectedPerson
             in (\person -> buildPersonItem action (isSelected person) person)

buildPersonItem ::
    (HasPath a) =>
    (Id Person -> a) ->
    Bool ->
    Person ->
    PersonItem a
buildPersonItem action isSelected person =
    PersonItem
        { selectionAction = action $ get #id person
        , activeClass = if isSelected then "active" else ""
        , ariaCurrent = if isSelected then "true" else "false"
        , firstName = get #firstName person
        , lastName = get #lastName person
        }

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
            {firstName} {lastName}
        </a>
    |]
