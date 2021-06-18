module Application.Timecard.Timecard where

import Application.Service.Validation (validateAndCreate)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ValidationSupport.ValidateField

newtype T = T
    { timecardEntries :: [TimecardEntry]
    }

validate :: Timecard -> Timecard
validate timecard =
    timecard
        |> validateField
            #weekOf
            (isInList [startOfWeek (get #weekOf timecard)])

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
                |> validateAndCreate validate

buildAll :: [TimecardEntry] -> [T]
buildAll timecardEntries =
    T . sortEntries <$> groupBy inSameWeek timecardEntries
  where
    sortEntries entries = sortBy dateCompare entries
    dateCompare entryA entryB = get #date entryA `compare` get #date entryB

buildForWeek :: Day -> [TimecardEntry] -> T
buildForWeek date timecardEntries =
    T sortedEntriesInWeek
  where
    sortedEntriesInWeek = sortBy dateCompare entriesInWeek
    entriesInWeek = filter (\timecardEntry -> entryWeek timecardEntry == week) timecardEntries
    week = weekOfYear date
    entryWeek entry = weekOfYear $ get #date entry
    dateCompare entryA entryB = get #date entryA `compare` get #date entryB

inSameWeek :: TimecardEntry -> TimecardEntry -> Bool
inSameWeek timecardEntry1 timecardEntry2 =
    let week1 = weekOfYear $ get #date timecardEntry1
     in let week2 = weekOfYear $ get #date timecardEntry2
         in week1 == week2

weekOfYear :: Day -> (Integer, Int)
weekOfYear day =
    let (year, week, _) = toWeekDate day
     in (year, week)

startOfWeek :: Day -> Day
startOfWeek day =
    let (year, week, _) = toWeekDate day
     in fromWeekDate year week 1
