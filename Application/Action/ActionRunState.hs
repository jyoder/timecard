module Application.Action.ActionRunState (
    updateNotStarted,
    updateSuspended,
    updateRunning,
    updateFinished,
    updateCanceled,
    updateFailed,
    validate,
    notStarted,
    suspended,
    running,
    finished,
    canceled,
    failed,
) where

import Application.Service.Validation (validateAndUpdate)
import Generated.Types
import IHP.Prelude
import IHP.ValidationSupport.ValidateField

updateNotStarted :: (?modelContext :: ModelContext) => ActionRunState -> IO ActionRunState
updateNotStarted = updateState notStarted

updateSuspended :: (?modelContext :: ModelContext) => ActionRunState -> IO ActionRunState
updateSuspended = updateState suspended

updateRunning :: (?modelContext :: ModelContext) => ActionRunState -> IO ActionRunState
updateRunning = updateState running

updateFinished :: (?modelContext :: ModelContext) => ActionRunState -> IO ActionRunState
updateFinished = updateState finished

updateCanceled :: (?modelContext :: ModelContext) => ActionRunState -> IO ActionRunState
updateCanceled = updateState canceled

updateFailed :: (?modelContext :: ModelContext) => ActionRunState -> IO ActionRunState
updateFailed = updateState failed

updateState :: (?modelContext :: ModelContext) => Text -> ActionRunState -> IO ActionRunState
updateState newState actionRunState =
    actionRunState |> set #state newState |> validateAndUpdate validate

validate :: ActionRunState -> ActionRunState
validate actionRunState =
    actionRunState
        |> validateField
            #state
            ( isInList
                [ notStarted
                , suspended
                , running
                , finished
                , canceled
                , failed
                ]
            )

notStarted :: Text
notStarted = "not_started"

suspended :: Text
suspended = "suspended"

running :: Text
running = "running"

finished :: Text
finished = "finished"

canceled :: Text
canceled = "canceled"

failed :: Text
failed = "failed"
