module Application.Brain.Act where

import qualified Application.Action.ActionRunState as ActionRunState
import Application.Brain.Decide as Decide
import qualified Application.Timecard.Entry as Entry
import qualified Application.Timecard.EntryRequest as EntryRequest
import Generated.Types
import IHP.ControllerPrelude
import IHP.Prelude

act :: (?modelContext :: ModelContext) => Decide.Plan -> IO ()
act Decide.SuspendScheduledMessages {..} = do
    actionRunState <- fetch actionRunStateId
    ActionRunState.updateSuspended actionRunState
    pure ()
act Decide.CreateTimecardEntry {..} = do
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
            workerId
            [linkedMessageId]
            timecardEntry

    case timecardEntry of
        Left _ -> pure ()
        Right timecardEntry -> do
            EntryRequest.scheduleNextRequest
                companyTimeZone
                now
                timecardEntry
                worker
                botPhoneNumberId
                workerPhoneNumberId
            pure ()
act DoNothing =
    pure ()