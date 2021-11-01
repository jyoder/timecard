module Application.Audit.Query where

import "string-interpolate" Data.String.Interpolate (i)
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

data Row = Row
    { createdAt :: !UTCTime
    , createdBy :: !(Maybe Text)
    , action :: !AuditAction
    , actionContext :: !Text
    }
    deriving (Eq, Show)

instance FromRow Row where
    fromRow =
        Row
            <$> field
            <*> field
            <*> field
            <*> field

fetchByPhoneNumber :: (?modelContext :: ModelContext) => Id PhoneNumber -> IO [Row]
fetchByPhoneNumber phoneNumberId =
    sqlQuery query (Only phoneNumberId)

query :: Query
query =
    [i|
        select
            audit_entries.created_at,
            users.email,
            audit_entries.action,
            audit_entries.action_context
        from
            audit_entries
            left join
                users on (audit_entries.user_id = users.id)
        where
            audit_entries.phone_number_id = ?
        order by
            audit_entries.created_at desc
        limit
            #{maxEntriesToRetrieve}
    |]
  where
    maxEntriesToRetrieve = 100 :: Int
