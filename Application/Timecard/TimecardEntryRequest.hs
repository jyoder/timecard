module Application.Timecard.TimecardEntryRequest (
    scheduleNextRequest,
    scheduledRequestExists,
    requestBody,
    nextRequestTime,
) where

import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Timecard.TimecardQueries as TimecardQueries
import Data.Time.Calendar.WeekDate (toWeekDate)
import Generated.Types
import IHP.ControllerPrelude
import Text.Read (read)

scheduleNextRequest ::
    (?modelContext :: ModelContext) =>
    TimeZone ->
    UTCTime ->
    TimecardEntry ->
    Person ->
    Id PhoneNumber ->
    Id PhoneNumber ->
    IO ()
scheduleNextRequest timeZone now newEntry person fromId toId = do
    alreadyScheduled <- scheduledRequestExists toId
    workerPreference <-
        query @WorkerPreference
            |> filterWhere (#personId, get #id person)
            |> fetchOne
    timecards <-
        TimecardQueries.fetchByPerson
            TimecardQueries.EntriesDateDescending
            (get #id person)

    let sendTimeOfDay = get #sendDailyReminderAt workerPreference
    let timecardEntries = maybe [] (get #entries) (head timecards)
    let timecardEntryDays = get #date <$> timecardEntries
    let body = requestBody person newEntry
    let maybeSendAt =
            nextRequestTime
                alreadyScheduled
                timeZone
                sendTimeOfDay
                timecardEntryDays
                now

    case maybeSendAt of
        Just sendAt -> SendMessageAction.schedule fromId toId body sendAt >> pure ()
        Nothing -> pure ()

scheduledRequestExists ::
    (?modelContext :: ModelContext) =>
    Id PhoneNumber ->
    IO Bool
scheduledRequestExists toPhoneNumberId = do
    sendMessageActions <- SendMessageAction.fetchFutureByPhoneNumber toPhoneNumberId
    pure $ not $ null sendMessageActions

requestBody :: Person -> TimecardEntry -> Text
requestBody person lastEntry =
    "Hey "
        <> get #goesBy person
        <> " - I've got you at "
        <> get #jobName lastEntry
        <> " today. Let me know what hours you worked and what you did when you have a chance. Thanks!"

nextRequestTime ::
    Bool ->
    TimeZone ->
    TimeOfDay ->
    [Day] ->
    UTCTime ->
    Maybe UTCTime
nextRequestTime
    alreadyScheduled
    timeZone
    requestTimeOfDay
    timecardEntryDays
    now
        | alreadyScheduled = Nothing
        | otherwise =
            requestTime'
                ( if isWeekend today || now' >= requestTimeToday
                    then nextTimecardEntryDay nextWorkingDay' timecardEntryDays
                    else nextTimecardEntryDay today timecardEntryDays
                )
      where
        requestTime' day = Just $ toUtc $ requestTime requestTimeOfDay day
        nextWorkingDay' = nextWorkingDay today
        requestTimeToday = requestTime requestTimeOfDay today
        today = localDay now'
        now' = utcToLocalTime timeZone now
        toUtc = localTimeToUTC timeZone

nextTimecardEntryDay :: Day -> [Day] -> Day
nextTimecardEntryDay nextWorkingDay entryDays =
    let openDays = (allDays \\ entryDays) \\ weekends
     in fromMaybe nextWorkingDay (head openDays)
  where
    allDays = [nextWorkingDay .. addDays 3 lastDay]
    weekends = filter isWeekend allDays
    lastDay = fromMaybe nextWorkingDay (last sortedRecentEntryDays)
    sortedRecentEntryDays = nub $ sort recentEntryDays
    recentEntryDays = filter (> nextWorkingDay) entryDays

requestTime :: TimeOfDay -> Day -> LocalTime
requestTime requestTimeOfDay day = LocalTime day requestTimeOfDay

nextWorkingDay :: Day -> Day
nextWorkingDay today =
    case toWeekDate today of
        (_, _, 5) -> addDays 3 today -- Friday we add 3 days to get to Monday
        (_, _, 6) -> addDays 2 today -- Saturday we add 2 days to get to Monday
        _ -> addDays 1 today -- All other days we need only look to tomorrow
