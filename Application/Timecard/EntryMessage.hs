module Application.Timecard.EntryMessage (
    createAll,
    replaceAll,
    fetchByTimecardEntry,
) where

import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

createAll ::
    (?modelContext :: ModelContext) =>
    Id TimecardEntry ->
    [Id TwilioMessage] ->
    IO [TimecardEntryMessage]
createAll timecardEntryId twilioMessageIds =
    let timecardEntryMessages = buildAll timecardEntryId twilioMessageIds
     in createMany timecardEntryMessages

replaceAll ::
    (?modelContext :: ModelContext) =>
    Id TimecardEntry ->
    [Id TwilioMessage] ->
    IO [TimecardEntryMessage]
replaceAll timecardEntryId twilioMessageIds =
    let timecardEntryMessages = buildAll timecardEntryId twilioMessageIds
     in withTransaction do
            oldTimecardEntryMessages <- fetchByTimecardEntry timecardEntryId
            deleteRecords oldTimecardEntryMessages
            createMany timecardEntryMessages

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