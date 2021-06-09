module Application.Service.ActionRunState (
    updateNotStarted,
    updateRunning,
    updateFinished,
    updateCanceled,
    updateFailed,
    validate,
    notStarted,
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
        |> validateField #state (isInList [notStarted, running, finished, canceled, failed])

notStarted :: Text
notStarted = "not_started"

running :: Text
running = "running"

finished :: Text
finished = "finished"

canceled :: Text
canceled = "canceled"

failed :: Text
failed = "failed"
