module Application.People.View (
    buildPeople,
    buildPerson,
    personState,
    Person (..),
    PersonState (..),
) where

import qualified Application.Action.ActionRunState as ActionRunState
import Application.People.Query (Row (..))
import qualified Generated.Types as Types
import IHP.Prelude

data Person = Person
    { id :: !(Id Types.Person)
    , firstName :: !Text
    , lastName :: !Text
    , goesBy :: !Text
    , state :: !PersonState
    }
    deriving (Eq, Show)

data PersonState
    = PersonIdle
    | PersonAutoPilot
    | PersonNeedsAttention
    deriving (Eq, Show)

buildPeople :: [Row] -> [Person]
buildPeople rows = buildPerson <$> rows

buildPerson :: Row -> Person
buildPerson row =
    Person
        { id = get #id row
        , firstName = get #firstName row
        , lastName = get #lastName row
        , goesBy = get #goesBy row
        , state = personState $ get #sendMessageActionState row
        }

personState :: Maybe Text -> PersonState
personState state
    | state == Just ActionRunState.notStarted = PersonAutoPilot
    | state == Just ActionRunState.suspended = PersonNeedsAttention
    | otherwise = PersonIdle
