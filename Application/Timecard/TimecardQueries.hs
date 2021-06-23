module Application.Timecard.TimecardQueries (
    Timecard (..),
    Status (..),
    TimecardEntry (..),
    AccessToken (..),
    EntriesSort (..),
    fetchByPerson,
    fetchById,
    fetchByPersonAndWeek,
) where

import Control.Exception.Safe (throwString)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import "string-interpolate" Data.String.Interpolate (i)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import qualified Generated.Types as Types
import IHP.ModelSupport
import IHP.Prelude

data Timecard = Timecard
    { id :: !(Id Types.Timecard)
    , personId :: !(Id Types.Person)
    , weekOf :: !Day
    , status :: !Status
    , entries :: ![TimecardEntry]
    }

data Status
    = TimecardInProgress
    | TimecardReadyForReview
    | TimecardUnderReview !AccessToken
    | TimecardSigned !Signing

data TimecardEntry = TimecardEntry
    { id :: !(Id Types.TimecardEntry)
    , date :: !Day
    , jobName :: !Text
    , hoursWorked :: !Double
    , workDone :: !Text
    , invoiceTranslation :: !Text
    }

data AccessToken = AccessToken
    { id :: !(Id Types.AccessToken)
    , value :: !Text
    , expiresAt :: !UTCTime
    , isRevoked :: !Bool
    }

data Signing = Signing
    { id :: !(Id Types.Signing)
    , signedAt :: !UTCTime
    }

data EntriesSort
    = EntriesDateAscending
    | EntriesDateDescending

data ResultRow = ResultRow
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
    , timecardEntryHoursWorked :: !Double
    , timecardEntryWorkDone :: !Text
    , timecardEntryInvoiceTranslation :: !Text
    }

instance FromRow ResultRow where
    fromRow =
        ResultRow
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

fetchByPerson ::
    (?modelContext :: ModelContext) =>
    EntriesSort ->
    Id Types.Person ->
    IO [Timecard]
fetchByPerson entriesSort personId =
    sqlQuery (fetchByPersonQuery entriesSort) (Only personId)
        <&> buildTimecards

fetchByPersonQuery :: EntriesSort -> Query
fetchByPersonQuery entriesSort =
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
            timecards.person_id = ?
        order by
            timecards.week_of desc,
            timecard_entries.date #{sort entriesSort},
            timecard_entries.created_at #{sort entriesSort};
    |]
  where
    sort EntriesDateAscending = "asc" :: Text
    sort EntriesDateDescending = "desc"

fetchById ::
    (?modelContext :: ModelContext) =>
    EntriesSort ->
    Id Types.Timecard ->
    IO Timecard
fetchById entriesSort timecardId = do
    timecards <-
        sqlQuery (fetchByIdQuery entriesSort) (Only timecardId)
            <&> buildTimecards

    case timecards of
        (timecard : _) -> pure timecard
        _ -> throwString "Timecard not found"

fetchByIdQuery :: EntriesSort -> Query
fetchByIdQuery entriesSort =
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
            timecards.id = ?
        order by
            timecard_entries.date #{sort entriesSort};
    |]
  where
    sort EntriesDateAscending = "asc" :: Text
    sort EntriesDateDescending = "desc"

-- TODO: remove
fetchByPersonAndWeek ::
    (?modelContext :: ModelContext) =>
    EntriesSort ->
    Id Types.Person ->
    Day ->
    IO Timecard
fetchByPersonAndWeek entriesSort personId day = do
    timecards <-
        sqlQuery (fetchByPersonAndWeekQuery entriesSort) (personId, day)
            <&> buildTimecards

    case timecards of
        (timecard : _) -> pure timecard
        _ -> throwString "Timecard not found"

fetchByPersonAndWeekQuery :: EntriesSort -> Query
fetchByPersonAndWeekQuery entriesSort =
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
            timecards.person_id = ?
            and timecards.week_of = date_trunc('week', (?)::date)
        order by
            timecard_entries.date #{sort entriesSort};
    |]
  where
    sort EntriesDateAscending = "asc" :: Text
    sort EntriesDateDescending = "desc"

buildTimecards :: [ResultRow] -> [Timecard]
buildTimecards queryRows =
    (buildTimecard <$> groupBy compareRows queryRows) |> catMaybes
  where
    compareRows row1 row2 = get #timecardId row1 == get #timecardId row2

buildTimecard :: [ResultRow] -> Maybe Timecard
buildTimecard [] = Nothing
buildTimecard (firstRow : otherRows) =
    Just
        Timecard
            { id = get #timecardId firstRow
            , personId = get #timecardPersonId firstRow
            , weekOf = get #timecardWeekOf firstRow
            , status = buildStatus (firstRow :| otherRows)
            , entries = buildTimecardEntry <$> firstRow : otherRows
            }

buildStatus :: NonEmpty ResultRow -> Status
buildStatus resultRows =
    let (id, value, expiresAt, isRevoked, signingId, signedAt) =
            ( get #accessTokenId firstRow
            , get #accessTokenValue firstRow
            , get #accessTokenExpiresAt firstRow
            , get #accessTokenIsRevoked firstRow
            , get #signingId firstRow
            , get #signingSignedAt firstRow
            )
     in case (id, value, expiresAt, isRevoked, signingId, signedAt) of
            (Just id, Just value, Just expiresAt, Just isRevoked, Nothing, Nothing) ->
                TimecardUnderReview $ AccessToken {..}
            (_, _, _, _, Just signingId, Just signedAt) ->
                TimecardSigned $ Signing signingId signedAt
            _ ->
                if isTimecardComplete resultRows
                    then TimecardReadyForReview
                    else TimecardInProgress
  where
    firstRow = NE.head resultRows
    otherRows = NE.tail resultRows

buildTimecardEntry :: ResultRow -> TimecardEntry
buildTimecardEntry resultRow =
    TimecardEntry
        { id = get #timecardEntryId resultRow
        , date = get #timecardEntryDate resultRow
        , jobName = get #timecardEntryJobName resultRow
        , hoursWorked = get #timecardEntryHoursWorked resultRow
        , workDone = get #timecardEntryWorkDone resultRow
        , invoiceTranslation = get #timecardEntryInvoiceTranslation resultRow
        }

isTimecardComplete :: NonEmpty ResultRow -> Bool
isTimecardComplete resultRows =
    startsWithMonday uniqueSortedDays
        && daysAreConsecutive uniqueSortedDays
        && length uniqueSortedDays == daysInWorkWeek
  where
    uniqueSortedDays = days |> NE.sort |> NE.nub
    days = get #timecardEntryDate <$> resultRows
    daysInWorkWeek = 5

startsWithMonday :: NonEmpty Day -> Bool
startsWithMonday uniqueSortedDays =
    let (_, _, dayOfWeek) = toWeekDate $ NE.head uniqueSortedDays
     in dayOfWeek == 1

daysAreConsecutive :: NonEmpty Day -> Bool
daysAreConsecutive uniqueSortedDays =
    all (uncurry (==)) (zip [firstDay ..] (NE.toList uniqueSortedDays))
  where
    firstDay = NE.head uniqueSortedDays
