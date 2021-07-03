module Application.Timecard.Timecard (
    validate,
    fetchOrCreate,
) where

import Application.Service.Time (startOfWeek)
import Application.Service.Validation (validateAndCreateIO)
import Generated.Types
import IHP.ControllerPrelude

validate :: (?modelContext :: ModelContext) => Timecard -> IO Timecard
validate timecard =
    timecard
        |> validateField
            #weekOf
            ( isInList [startOfWeek (get #weekOf timecard)]
                |> withCustomErrorMessage "weekOf must be a Monday, the start of the week"
            )
        |> validateFieldIO #weekOf (matchesTimecardEntries $ get #id timecard)

matchesTimecardEntries ::
    (?modelContext :: ModelContext) =>
    Id Timecard ->
    Day ->
    IO ValidatorResult
matchesTimecardEntries timecardId weekOf = do
    maybeTimecardEntry <-
        query @TimecardEntry
            |> filterWhere (#timecardId, timecardId)
            |> fetchOneOrNothing

    pure $ case maybeTimecardEntry of
        Just timecardEntry ->
            if startOfWeek (get #date timecardEntry) == weekOf
                then Success
                else Failure "weekOf must match timecard entries"
        Nothing ->
            Success

fetchOrCreate ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    Day ->
    IO Timecard
fetchOrCreate personId day = do
    let weekOf = startOfWeek day

    maybeTimecard <-
        query @Timecard
            |> filterWhere (#personId, personId)
            |> filterWhere (#weekOf, weekOf)
            |> fetchOneOrNothing

    case maybeTimecard of
        Just timecard -> pure timecard
        Nothing -> do
            newRecord @Timecard
                |> set #personId personId
                |> set #weekOf weekOf
                |> validateAndCreateIO validate
