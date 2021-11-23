module Application.Reports.AutomationQuery (
    Row (..),
    AutomationStatus (..),
    ReportInterval (..),
    fetch,
) where

import "string-interpolate" Data.String.Interpolate (i)
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import qualified Generated.Types as Types
import IHP.ModelSupport
import IHP.Prelude
import Text.Read (read)

data Row = Row
    { date :: !Day
    , personId :: !(Id Types.Person)
    , personFirstName :: !Text
    , personLastName :: !Text
    , automationStatus :: AutomationStatus
    }
    deriving (Eq, Show)

data AutomationStatus
    = FullyAutomated
    | NotFullyAutomated
    | NoActivity
    deriving (Eq, Show)

data ReportInterval
    = ByDay
    | ByWeek
    deriving (Eq, Show)

instance FromRow Row where
    fromRow =
        Row
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field

instance FromField AutomationStatus where
    fromField field maybeData =
        case maybeData of
            Just "fully_automated" -> pure FullyAutomated
            Just "not_fully_automated" -> pure NotFullyAutomated
            Just "no_activity" -> pure NoActivity
            Just _data -> returnError ConversionFailed field (cs _data)
            Nothing -> returnError UnexpectedNull field ""

fetch :: (?modelContext :: ModelContext) => ReportInterval -> Day -> IO [Row]
fetch reportInterval asOf = sqlQuery (query reportInterval) (Only asOf)

query :: ReportInterval -> Query
query reportInterval =
    [i|
        #{datesSql reportInterval},
        people_by_date as (
            select
                dates.date date,
                people.id id,
                people.first_name first_name,
                people.last_name last_name
            from
                worker_settings,
                dates cross join people
            where
                worker_settings.person_id = people.id

        ),
        automations as (
            select
                people.id person_id,
                #{auditEntryDateSql reportInterval} audit_entry_date,
                audit_entries.user_id audit_entry_user_id
            from
                people,
                phone_numbers,
                phone_contacts,
                audit_entries
            where
                phone_contacts.person_id = people.id
                and phone_contacts.phone_number_id = phone_numbers.id
                and audit_entries.phone_number_id = phone_numbers.id
        )
        select
            people_by_date.date,
            people_by_date.id,
            people_by_date.first_name,
            people_by_date.last_name,
            (case
                when count(automations.person_id) > 0
                    then
                        (case
                            when every(automations.audit_entry_user_id is null)
                                then 'fully_automated'
                                else 'not_fully_automated'
                        end)
                    else
                        'no_activity'
            end) automation_status
        from
            people_by_date left join
                automations on (
                    people_by_date.id = automations.person_id
                    and people_by_date.date = automations.audit_entry_date
                )
        group by
            people_by_date.id,
            people_by_date.last_name,
            people_by_date.first_name,
            people_by_date.date
        order by
            people_by_date.last_name asc,
            people_by_date.first_name asc,
            people_by_date.id asc,
            people_by_date.date desc;
    |]

datesSql :: ReportInterval -> Text
datesSql reportInterval =
    [i|
        with dates as (
            select
                date::date
            from
                generate_series('#{show startDate}', ?, '#{show interval} day'::interval) date
        )
    |]
  where
    interval = case reportInterval of
        ByDay -> 1
        ByWeek -> 7

auditEntryDateSql :: ReportInterval -> Text
auditEntryDateSql reportInterval =
    case reportInterval of
        ByDay ->
            [i|
                audit_entries.created_at::date
            |]
        ByWeek ->
            [i|
                audit_entries.created_at::date - ((6 + cast(extract(dow from audit_entries.created_at::date) as int)) % 7)
            |]

-- This is the date when we first began collecting audit information.
startDate :: Day
startDate = read "2021-10-25"