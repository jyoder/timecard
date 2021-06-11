module Application.Timecard.TimecardEntry (
    validate,
    fetchByPerson,
) where

import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ValidationSupport.ValidateField

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