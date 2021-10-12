module Application.Timecard.EntryMessage (
    createAll,
    replaceAll,
    deleteAll,
    fetchByTimecardEntry,
) where

import Application.Service.Transaction (withTransactionOrSavepoint)
import Generated.Types
import IHP.Fetch
import IHP.ModelSupport hiding (deleteAll)
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
     in withTransactionOrSavepoint do
            oldTimecardEntryMessages <- fetchByTimecardEntry timecardEntryId
            deleteRecords oldTimecardEntryMessages
            createMany timecardEntryMessages

deleteAll ::
    (?modelContext :: ModelContext) =>
    Id TimecardEntry ->
    IO ()
deleteAll timecardEntryId =
    withTransactionOrSavepoint do
        timecardEntryMessages <-
            query @TimecardEntryMessage
                |> filterWhere (#timecardEntryId, timecardEntryId)
                |> fetch
        deleteRecords timecardEntryMessages

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