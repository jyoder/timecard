module Application.Timecard.TimecardEntryMessage (
    replaceAllForTimecard,
    fetchByTimecardEntry,
) where

import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

replaceAllForTimecard ::
    (?modelContext :: ModelContext) =>
    Id TimecardEntry ->
    [Id TwilioMessage] ->
    IO ()
replaceAllForTimecard timecardEntryId twilioMessageIds =
    let timecardEntryMessages = buildAll timecardEntryId twilioMessageIds
     in withTransaction do
            oldTimecardEntryMessages <- fetchByTimecardEntry timecardEntryId
            deleteRecords oldTimecardEntryMessages
            mapM_ createRecord timecardEntryMessages

fetchByTimecardEntry ::
    (?modelContext :: ModelContext) =>
    Id TimecardEntry ->
    IO [TimecardEntryMessage]
fetchByTimecardEntry timecardEntryId = do
    query @TimecardEntryMessage
        |> filterWhere (#timecardEntryId, timecardEntryId)
        |> fetch

buildAll ::
    (?modelContext :: ModelContext) =>
    Id TimecardEntry ->
    [Id TwilioMessage] ->
    [TimecardEntryMessage]
buildAll timecardEntryId =
    map $ \messageId ->
        newRecord @TimecardEntryMessage
            |> set #timecardEntryId timecardEntryId
            |> set #twilioMessageId messageId