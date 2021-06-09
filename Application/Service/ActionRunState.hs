module Application.Service.ActionRunState (
    updateNotStarted,
    updateRunning,
    updateFinished,
    updateFailed,
    validate,
    notStarted,
    running,
    finished,
    failed,
) where

import Application.Service.Validation (validateAndUpdate)
import Generated.Types
import IHP.Prelude
import IHP.ValidationSupport.ValidateField

updateNotStarted :: (?modelContext :: ModelContext) => ActionRunState -> IO ActionRunState
updateNotStarted actionRunState =
    actionRunState |> set #state notStarted |> validateAndUpdate validate

updateRunning :: (?modelContext :: ModelContext) => ActionRunState -> IO ActionRunState
updateRunning actionRunState =
    actionRunState |> set #state running |> validateAndUpdate validate

updateFinished :: (?modelContext :: ModelContext) => ActionRunState -> IO ActionRunState
updateFinished actionRunState =
    actionRunState |> set #state notStarted |> validateAndUpdate validate

updateFailed :: (?modelContext :: ModelContext) => ActionRunState -> IO ActionRunState
updateFailed actionRunState =
    actionRunState |> set #state notStarted |> validateAndUpdate validate

validate :: ActionRunState -> ActionRunState
validate actionRunState =
    actionRunState
        |> validateField #state (isInList [notStarted, running, finished, failed])

notStarted :: Text
notStarted = "not_started"

running :: Text
running = "running"

finished :: Text
finished = "finished"

failed :: Text
failed = "failed"
