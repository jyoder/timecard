module Application.Brain.Act where

import qualified Application.Action.ActionRunState as ActionRunState
import Application.Brain.Decide as Decide
import qualified Application.Timecard.Entry as Entry
import qualified Application.Timecard.EntryRequest as EntryRequest
import qualified Application.Timecard.Query as Timecard.Query
import qualified Application.Timecard.ReviewRequest as ReviewRequest
import qualified Application.Timecard.View as Timecard.View
import Generated.Types
import IHP.ControllerPrelude

act :: (?modelContext :: ModelContext) => Text -> Decide.Plan -> IO ()
act _ Decide.SuspendScheduledMessages {..} = do
    actionRunState <- fetch actionRunStateIds
    mapM_ ActionRunState.updateSuspended actionRunState
act baseUrl Decide.CreateTimecardEntry {..} = do
    worker <- fetch workerId

    let timecardEntry =
            newRecord @TimecardEntry
                |> set #date date
                |> set #jobName jobName
                |> set #clockedInAt clockedInAt
                |> set #clockedOutAt clockedOutAt
                |> set #lunchDuration (Just lunchDuration)
                |> set #hoursWorked hoursWorked
                |> set #workDone workDone
                |> set #invoiceTranslation invoiceTranslation

    timecardEntry <-
        Entry.create
            Nothing
            workerId
            workerPhoneNumberId
            [linkedMessageId]
            timecardEntry

    case timecardEntry of
        Right timecardEntry -> do
            EntryRequest.scheduleNextRequest
                companyTimeZone
                now
                timecardEntry
                worker
                botPhoneNumberId
                workerPhoneNumberId

            let timecardId = get #timecardId timecardEntry
            readyForReview <- isTimecardReadyForReview timecardId
            when
                readyForReview
                ( ReviewRequest.scheduleRequest
                    Nothing
                    baseUrl
                    now
                    timecardId
                    worker
                    botPhoneNumberId
                    workerPhoneNumberId
                    >> pure ()
                )
        Left _ -> pure ()
act _ DoNothing =
    pure ()

isTimecardReadyForReview :: (?modelContext :: ModelContext) => Id Timecard -> IO Bool
isTimecardReadyForReview timecardId = do
    rows <- Timecard.Query.fetchById Timecard.Query.EntriesDateAscending timecardId
    case Timecard.View.buildTimecard rows of
        Just timecard ->
            pure $ get #status timecard == Timecard.View.TimecardReadyForReview
        Nothing ->
            pure False
