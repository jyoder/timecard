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
        with latest_timecards as (
            #{latestTimecardsQuerySql whereCondition}
        ), 
        tokens as (
            #{tokensQuerySql}
        ),
        current_access_token as (
            #{currentAccessTokenQuerySql}
        ),
        signings_ as (
            #{signingsQuerySql}
        ),
        latest_signing as (
            #{latestSigningQuerySql}
        )
        select
            latest_timecards.id timecard_id,
            latest_timecards.person_id timecard_person_id,
            latest_timecards.week_of timecard_week_of,
            current_access_token.access_token_id access_token_id,
            current_access_token.access_token_value access_token_value,
            current_access_token.access_token_expires_at access_token_expires_at,
            current_access_token.access_token_is_revoked access_token_is_revoked,
            latest_signing.signing_id signing_id,
            latest_signing.signing_signed_at signing_signed_at,
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
            latest_timecards
            inner join
                timecard_entries on (timecard_entries.timecard_id = latest_timecards.id)
            left join
                current_access_token on (current_access_token.timecard_id = latest_timecards.id)
            left join
                latest_signing on (latest_signing.timecard_id = latest_timecards.id)
        order by
            latest_timecards.week_of desc,
            timecard_entries.date #{entriesSortSql entriesSort},
            timecard_entries.created_at #{entriesSortSql entriesSort}
    |]

tokensQuerySql :: Text
tokensQuerySql =
    [i|
        select
            latest_timecards.id timecard_id,
            access_tokens.id access_token_id,
            access_tokens.value access_token_value,
            access_tokens.expires_at access_token_expires_at,
            access_tokens.is_revoked access_token_is_revoked,
            row_number() over (
                partition by 
                    latest_timecards.id
                order by
                    access_tokens.is_revoked asc,
                    access_tokens.expires_at desc,
                    access_tokens.created_at desc
            ) as row_number
        from
            latest_timecards,
            timecard_access_tokens,
            access_tokens
        where
            timecard_access_tokens.timecard_id = latest_timecards.id
            and timecard_access_tokens.access_token_id = access_tokens.id
    |]

latestTimecardsQuerySql :: WhereCondition -> Text
latestTimecardsQuerySql whereCondition =
    [i|
        select
            timecards.*
        from
            timecards
        where
            #{whereConditionSql whereCondition}
        order by
            timecards.week_of desc
        limit
            #{maxTimecardsToRetrieve}
    |]
  where
    maxTimecardsToRetrieve = 100 :: Int

currentAccessTokenQuerySql :: Text
currentAccessTokenQuerySql =
    [i|
        select
            tokens.*
        from
            tokens
        where
            tokens.row_number = 1
    |]

signingsQuerySql :: Text
signingsQuerySql =
    [i|
        select
            latest_timecards.id timecard_id,
            signings.id signing_id,
            signings.signed_at signing_signed_at,
            row_number() over (
                partition by 
                    latest_timecards.id
                order by
                    signings.created_at desc,
                    signings.id asc
            ) as row_number
        from
            latest_timecards,
            timecard_signings,
            signings
        where
            timecard_signings.timecard_id = latest_timecards.id
            and timecard_signings.signing_id = signings.id
    |]

latestSigningQuerySql :: Text
latestSigningQuerySql =
    [i|
        select
            signings_.*
        from
            signings_
        where
            signings_.row_number = 1
    |]

whereConditionSql :: WhereCondition -> Text
whereConditionSql PersonIdCondition = "timecards.person_id = ?"
whereConditionSql TimecardIdCondition = "timecards.id = ?"

entriesSortSql :: EntriesSort -> Text
entriesSortSql EntriesDateAscending = "asc"
entriesSortSql EntriesDateDescending = "desc"
