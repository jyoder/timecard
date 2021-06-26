module Application.Timecard.TimecardSigning (
    create,
    validate,
) where

import Application.Service.Validation
import Generated.Types
import IHP.ControllerPrelude hiding (create)

create ::
    (?modelContext :: ModelContext) =>
    Id Timecard ->
    Id Signing ->
    IO TimecardSigning
create timecardId signingId =
    newRecord @TimecardSigning
        |> set #timecardId timecardId
        |> set #signingId signingId
        |> validateAndCreate validate

validate :: TimecardSigning -> TimecardSigning
validate timecardSigning = timecardSigning
