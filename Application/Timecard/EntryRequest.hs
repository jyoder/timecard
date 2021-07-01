module Application.Timecard.EntryRequest (
    nextRequestTime,
    scheduleNextRequest,
    requestBody,
) where

import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Timecard.Query as Timecard.Query
import Data.Time.Calendar.WeekDate (toWeekDate)
import Generated.Types
import IHP.ControllerPrelude

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
        | isWeekend today || now' >= requestTimeToday = Just $ nextTimecardEntryTime nextWorkingDay'
        | otherwise = Just $ nextTimecardEntryTime today
      where
        nextTimecardEntryTime startDay = requestTime' $ nextTimecardEntryDay startDay timecardEntryDays
        requestTime' day = toUtc $ requestTime requestTimeOfDay day
        nextWorkingDay' = nextWorkingDay today
        requestTimeToday = requestTime requestTimeOfDay today
        today = localDay now'
        now' = utcToLocalTime timeZone now
        toUtc = localTimeToUTC timeZone

scheduleNextRequest ::
    (?modelContext :: ModelContext) =>
    TimeZone ->
    UTCTime ->
    TimecardEntry ->
    Person ->
    Id PhoneNumber ->
    Id PhoneNumber ->
    IO (Maybe SendMessageAction)
scheduleNextRequest timeZone now newEntry person fromId toId = do
    alreadyScheduled <- scheduledRequestExists now toId
    workerPreference <-
        query @WorkerPreference
            |> filterWhere (#personId, get #id person)
            |> fetchOne
    timecardEntryRows <-
        Timecard.Query.fetchRowsByPerson
            Timecard.Query.EntriesDateDescending
            (get #id person)

    let sendTimeOfDay = get #sendDailyReminderAt workerPreference
    let timecardEntryDays = get #timecardEntryDate <$> timecardEntryRows
    let body = requestBody person newEntry
    let maybeSendAt =
            nextRequestTime
                alreadyScheduled
                timeZone
                sendTimeOfDay
                timecardEntryDays
                now

    case maybeSendAt of
        Just sendAt -> do
            sendMessageAction <- SendMessageAction.schedule fromId toId body sendAt
            pure $ Just sendMessageAction
        Nothing ->
            pure Nothing

requestBody :: Person -> TimecardEntry -> Text
requestBody person lastEntry =
    "Hey "
        <> get #goesBy person
        <> " - I've got you at "
        <> get #jobName lastEntry
        <> " today. Let me know what hours you worked and what you did when you have a chance. Thanks!"

scheduledRequestExists ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    Id PhoneNumber ->
    IO Bool
scheduledRequestExists asOf toPhoneNumberId = do
    sendMessageActions <- SendMessageAction.fetchAfterByPhoneNumber asOf toPhoneNumberId
    pure $ not $ null sendMessageActions

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
