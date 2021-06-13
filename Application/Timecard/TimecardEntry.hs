module Application.Timecard.TimecardEntry (
    validate,
    fetchByPerson,
    fetchByPersonAndWeek,
) where

import Database.PostgreSQL.Simple (Query)
import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ValidationSupport.ValidateField
import Text.RawString.QQ (r)

validate :: TimecardEntry -> TimecardEntry
validate timecardEntry =
    timecardEntry
        |> validateField #jobName nonEmpty
        |> validateField #hoursWorked (validateAny [isInList [0.0], isGreaterThan 0.0])
        |> validateField #workDone nonEmpty
        |> validateField #invoiceTranslation nonEmpty

fetchByPerson ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO [TimecardEntry]
fetchByPerson personId =
    query @TimecardEntry
        |> filterWhere (#personId, personId)
        |> orderByDesc #date
        |> fetch

fetchByPersonAndWeek ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    Day ->
    IO [TimecardEntry]
fetchByPersonAndWeek person day =
    sqlQuery fetchByPersonAndWeekQuery (person, day)

fetchByPersonAndWeekQuery :: Query
fetchByPersonAndWeekQuery =
    [r|
select
    timecard_entries.*
from
    timecard_entries
where
    timecard_entries.person_id = ?
    and date_trunc('week', timecard_entries.date) = date_trunc('week', (?)::date)
order by
    date desc;
    |]
