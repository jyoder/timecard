module Application.Timecard.TimecardEntryRequest (
    scheduleNextRequest,
    scheduleRequest,
    scheduledRequestExists,
    requestBody,
    nextRequestTime,
) where

import qualified Application.Action.SendMessageAction as SendMessageAction
import Data.Time.Calendar.WeekDate (toWeekDate)
import Generated.Types
import IHP.ControllerPrelude
import IHP.ModelSupport
import IHP.Prelude

scheduleNextRequest ::
    (?modelContext :: ModelContext) =>
    TimeZone ->
    TimecardEntry ->
    Person ->
    Id PhoneNumber ->
    Id PhoneNumber ->
    IO ()
scheduleNextRequest timeZone lastEntry person fromId toId = do
    now <- getCurrentTime
    alreadyScheduled <- scheduledRequestExists toId
    workerPreference <- query @WorkerPreference |> filterWhere (#personId, get #id person) |> fetchOne
    let sendTimeOfDay = get #sendDailyReminderAt workerPreference

    let body = requestBody person lastEntry
    let sendAt = nextRequestTime timeZone sendTimeOfDay now

    if not alreadyScheduled
        then SendMessageAction.schedule fromId toId body sendAt >> pure ()
        else pure ()

scheduleRequest ::
    (?modelContext :: ModelContext) =>
    UTCTime ->
    Id PhoneNumber ->
    Id PhoneNumber ->
    Text ->
    IO ()
scheduleRequest sendAt fromId toId body = do
    alreadyScheduled <- scheduledRequestExists toId
    if not alreadyScheduled
        then SendMessageAction.schedule fromId toId body sendAt >> pure ()
        else pure ()

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

nextRequestTime :: TimeZone -> TimeOfDay -> UTCTime -> UTCTime
nextRequestTime timeZone requestTimeOfDay now =
    if not (isWeekend $ localDay localTime) && localTime < requestTimeToday requestTimeOfDay localTime
        then requestTimeToday requestTimeOfDay localTime |> toUtc
        else requestTimeNextWorkingDay requestTimeOfDay localTime |> toUtc
  where
    localTime = utcToLocalTime timeZone now
    toUtc = localTimeToUTC timeZone

requestTimeToday :: TimeOfDay -> LocalTime -> LocalTime
requestTimeToday requestTimeOfDay now = LocalTime (localDay now) requestTimeOfDay

requestTimeNextWorkingDay :: TimeOfDay -> LocalTime -> LocalTime
requestTimeNextWorkingDay requestTimeOfDay now =
    LocalTime (nextWorkingDay $ localDay now) requestTimeOfDay

nextWorkingDay :: Day -> Day
nextWorkingDay today =
    case toWeekDate today of
        (_, _, 5) -> addDays 3 today -- Friday we add 3 days to get to Monday
        (_, _, 6) -> addDays 2 today -- Saturday we add 2 days to get to Monday
        _ -> addDays 1 today -- All other days we need only look to tomorrow
