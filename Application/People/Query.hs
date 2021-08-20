module Application.People.Query (
    fetchActiveWorkers,
    Row (..),
) where

import Application.People.Person (botGoesBy)
import Data.ByteString.UTF8 (toString)
import "string-interpolate" Data.String.Interpolate (i)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.FromRow (field, fromRow)
import qualified Generated.Types as Types
import IHP.ModelSupport
import IHP.Prelude

data Row = Row
    { id :: !(Id Types.Person)
    , firstName :: !Text
    , lastName :: !Text
    , goesBy :: !Text
    , sendMessageActionState :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance FromRow Row where
    fromRow =
        Row
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field

fetchActiveWorkers :: (?modelContext :: ModelContext) => IO [Row]
fetchActiveWorkers = do
    trackTableRead "people"
    trackTableRead "worker_settings"
    trackTableRead "action_run_states"
    trackTableRead "send_message_actions"
    sqlQuery query ()

query :: Query
query =
    [i|
        select
            people.id,
            people.first_name,
            people.last_name,
            people.goes_by,
            max(action_run_states.state) send_message_action_state
        from
            people
            inner join
                worker_settings on (worker_settings.person_id = people.id)
            inner join
                phone_contacts on (phone_contacts.person_id = people.id)
            left join
                send_message_actions on (send_message_actions.to_id = phone_contacts.phone_number_id)
            left join
                action_run_states on (
                    action_run_states.id = send_message_actions.action_run_state_id 
                    and (action_run_states.state = 'not_started' or action_run_states.state = 'suspended')
                )
        where
            people.goes_by <> '#{botGoesBy}'
            and worker_settings.is_active
            and people.first_name <> 'Matt'
            and people.last_name <> 'Killam'
        group by
            people.id
        order by
            people.last_name,
            people.first_name;
    |]
