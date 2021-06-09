module Application.Service.TimecardEntryMessage (
    buildAll,
    fetchByTimecardEntry,
) where

import Generated.Types
import IHP.Fetch
import IHP.ModelSupport
import IHP.Prelude
import IHP.QueryBuilder

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

fetchByTimecardEntry ::
    (?modelContext :: ModelContext) =>
    Id TimecardEntry ->
    IO [TimecardEntryMessage]
fetchByTimecardEntry timecardEntryId = do
    query @TimecardEntryMessage
        |> filterWhere (#timecardEntryId, timecardEntryId)
        |> fetch