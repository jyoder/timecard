module Application.Timecard.EntryRequest (
    nextRequestTime,
    scheduleNextRequest,
    requestBody,
) where

import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Base.AuditEntry as AuditEntry
import qualified Application.Base.WorkerSettings as WorkerSettings
import Application.Service.Time (nextWorkingDay)
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
    alreadyScheduled <- scheduledRequestExists toId
    workerSetting <-
        query @WorkerSetting
            |> filterWhere (#personId, get #id person)
            |> fetchOne
    timecardEntryRows <-
        Timecard.Query.fetchByPerson
            Timecard.Query.EntriesDateDescending
            (get #id person)

    let sendTimeOfDay = get #sendDailyReminderAt workerSetting
    let preferredLanguage = WorkerSettings.toLanguage $ get #preferredLanguage workerSetting
    let timecardEntryDays = get #timecardEntryDate <$> timecardEntryRows
    let body = requestBody preferredLanguage person newEntry
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
            AuditEntry.createDailyReminderScheduledEntry sendMessageAction sendAt
            pure $ Just sendMessageAction
        Nothing ->
            pure Nothing

requestBody :: WorkerSettings.Language -> Person -> TimecardEntry -> Text
requestBody WorkerSettings.English person lastEntry =
    "Hey "
        <> get #goesBy person
        <> " - I've got you at "
        <> get #jobName lastEntry
        <> " today. Let me know what hours you worked and what you did when you have a chance. Thanks!"
requestBody WorkerSettings.Spanish person lastEntry =
    "¡Hola "
        <> get #goesBy person
        <> "! ¿Estabas en "
        <> get #jobName lastEntry
        <> " hoy? Hágame saber qué horas trabajó y qué hizo cuando tuvo la oportunidad. ¡Gracias!"

scheduledRequestExists ::
    (?modelContext :: ModelContext) =>
    Id PhoneNumber ->
    IO Bool
scheduledRequestExists toPhoneNumberId = do
    sendMessageActions <-
        SendMessageAction.fetchNotStartedOrSuspendedByPhoneNumber
            toPhoneNumberId
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
