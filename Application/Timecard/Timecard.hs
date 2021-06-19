module Application.Timecard.Timecard (
    T (..),
    EntriesSort (..),
    fetchOrCreate,
    fetchByPerson,
    fetchByPersonAndWeek,
    validate,
) where

import Application.Service.Time (startOfWeek)
import Application.Service.Validation (validateAndCreateIO)
import Generated.Types
import IHP.ControllerPrelude

data T = T
    { timecard :: !Timecard
    , entries :: ![TimecardEntry]
    }

data EntriesSort
    = EntriesAscending
    | EntriesDescending

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

fetchByPerson ::
    (?modelContext :: ModelContext) =>
    EntriesSort ->
    Id Person ->
    IO [T]
fetchByPerson entriesSort personId = do
    timecardEntries <-
        query @TimecardEntry
            |> innerJoin @Timecard (#timecardId, #id)
            |> filterWhereJoinedTable @Timecard (#personId, personId)
            |> fetch

    let timecardIds = nub $ get #timecardId <$> timecardEntries
    timecards <-
        query @Timecard
            |> filterWhereIn (#id, timecardIds)
            |> fetch

    (build entriesSort timecardEntries <$> timecards)
        |> sortBy compareDesc
        |> pure

fetchByPersonAndWeek ::
    (?modelContext :: ModelContext) =>
    EntriesSort ->
    Id Person ->
    Day ->
    IO T
fetchByPersonAndWeek entriesSort personId day = do
    timecardEntries <-
        query @TimecardEntry
            |> innerJoin @Timecard (#timecardId, #id)
            |> filterWhereJoinedTable @Timecard (#personId, personId)
            |> filterWhereJoinedTable @Timecard (#weekOf, startOfWeek day)
            |> fetch

    let timecardIds = nub $ get #timecardId <$> timecardEntries

    timecard <-
        query @Timecard
            |> filterWhereIn (#id, timecardIds)
            |> fetchOne

    pure $ build entriesSort timecardEntries timecard

validate :: (?modelContext :: ModelContext) => Timecard -> IO Timecard
validate timecard =
    timecard
        |> validateField #weekOf (isInList [startOfWeek (get #weekOf timecard)])
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

build :: EntriesSort -> [TimecardEntry] -> Timecard -> T
build entriesSort entries timecard =
    entries
        |> filterEntries
        |> sortBy (compareEntries entriesSort)
        |> T timecard
  where
    filterEntries entries = filter (belongsTo timecard) entries
    belongsTo timecard entry = get #timecardId entry == get #id timecard

compareDesc :: T -> T -> Ordering
compareDesc t1 t2 = weekOf t2 `compare` weekOf t1
  where
    weekOf timecard = get #timecard timecard |> get #weekOf

compareEntries :: EntriesSort -> TimecardEntry -> TimecardEntry -> Ordering
compareEntries EntriesAscending t1 t2 = get #date t1 `compare` get #date t2
compareEntries EntriesDescending t1 t2 = get #date t2 `compare` get #date t1
