module Application.Timecard.Query (
    Row (..),
    EntriesSort (..),
    fetchByPerson,
    fetchById,
) where

import "string-interpolate" Data.String.Interpolate (i)
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import qualified Generated.Types as Types
import IHP.ModelSupport
import IHP.Prelude

data EntriesSort
    = EntriesDateAscending
    | EntriesDateDescending

data WhereCondition
    = PersonIdCondition
    | TimecardIdCondition

data Row = Row
    { timecardId :: !(Id Types.Timecard)
    , timecardPersonId :: !(Id Types.Person)
    , timecardWeekOf :: !Day
    , accessTokenId :: !(Maybe (Id Types.AccessToken))
    , accessTokenValue :: !(Maybe Text)
    , accessTokenExpiresAt :: !(Maybe UTCTime)
    , accessTokenIsRevoked :: !(Maybe Bool)
    , signingId :: !(Maybe (Id Types.Signing))
    , signingSignedAt :: !(Maybe UTCTime)
    , timecardEntryId :: !(Id Types.TimecardEntry)
    , timecardEntryDate :: !Day
    , timecardEntryJobName :: !Text
    , timecardEntryClockedInAt :: !(Maybe TimeOfDay)
    , timecardEntryClockedOutAt :: !(Maybe TimeOfDay)
    , timecardEntryLunchDuration :: !(Maybe Int)
    , timecardEntryHoursWorked :: !Double
    , timecardEntryWorkDone :: !Text
    , timecardEntryInvoiceTranslation :: !Text
    }
    deriving (Show, Eq)

instance FromRow Row where
    fromRow =
        Row
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

fetchByPerson ::
    (?modelContext :: ModelContext) =>
    EntriesSort ->
    Id Types.Person ->
    IO [Row]
fetchByPerson entriesSort personId =
    sqlQuery (query PersonIdCondition entriesSort) (Only personId)

fetchById ::
    (?modelContext :: ModelContext) =>
    EntriesSort ->
    Id Types.Timecard ->
    IO [Row]
fetchById entriesSort timecardId =
    sqlQuery (query TimecardIdCondition entriesSort) (Only timecardId)

query :: WhereCondition -> EntriesSort -> Query
query whereCondition entriesSort =
    [i|
        select
            timecards.id timecard_id,
            timecards.person_id timecard_person_id,
            timecards.week_of timecard_week_of,
            access_tokens.id access_token_id,
            access_tokens.value access_token_value,
            access_tokens.expires_at access_token_expires_at,
            access_tokens.is_revoked access_token_is_revoked,
            signings.id signing_id,
            signings.signed_at signing_signed_at,
            timecard_entries.id timecard_entry_id,
            timecard_entries.date timecard_entry_date,
            timecard_entries.job_name timecard_entry_job_name,
            timecard_entries.clocked_in_at timecard_entry_clocked_in_at,
            timecard_entries.clocked_out_at timecard_entry_clocked_out_at,
            timecard_entries.lunch_duration timecard_entry_lunch_duration,
            timecard_entries.hours_worked timecard_entry_hours_worked,
            timecard_entries.work_done timecard_entry_work_done,
            timecard_entries.invoice_translation timecard_entry_invoice_translation
        from
            timecards
            inner join
                timecard_entries on (timecard_entries.timecard_id = timecards.id)
            left join
                timecard_access_tokens on (timecard_access_tokens.timecard_id = timecards.id)
            left join
                access_tokens on (timecard_access_tokens.access_token_id = access_tokens.id)
            left join
                timecard_signings on (timecard_signings.timecard_id = timecards.id)
            left join
                signings on (timecard_signings.signing_id = signings.id)
        where 
            #{whereConditionSql whereCondition}
        order by
            timecards.week_of desc,
            timecard_entries.date #{entriesSortSql entriesSort},
            timecard_entries.created_at #{entriesSortSql entriesSort}
    |]

whereConditionSql :: WhereCondition -> Text
whereConditionSql PersonIdCondition = "timecards.person_id = ?"
whereConditionSql TimecardIdCondition = "timecards.id = ?"

entriesSortSql :: EntriesSort -> Text
entriesSortSql EntriesDateAscending = "asc"
entriesSortSql EntriesDateDescending = "desc"
