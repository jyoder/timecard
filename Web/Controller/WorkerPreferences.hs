module Web.Controller.WorkerPreferences where

import Web.Controller.Prelude
import Web.View.WorkerPreferences.Edit
import Web.View.WorkerPreferences.Index
import Web.View.WorkerPreferences.New
import Web.View.WorkerPreferences.Show

instance Controller WorkerPreferencesController where
    beforeAction = ensureIsUser

    action WorkerPreferencesAction = do
        workerPreferences <- query @WorkerPreference |> fetch
        render IndexView {..}
    --
    action NewWorkerPreferenceAction = do
        let workerPreference = newRecord
        render NewView {..}
    --
    action ShowWorkerPreferenceAction {workerPreferenceId} = do
        workerPreference <- fetch workerPreferenceId
        render ShowView {..}
    --
    action EditWorkerPreferenceAction {workerPreferenceId} = do
        workerPreference <- fetch workerPreferenceId
        render EditView {..}
    --
    action UpdateWorkerPreferenceAction {workerPreferenceId} = do
        workerPreference <- fetch workerPreferenceId
        workerPreference
            |> buildWorkerPreference
            |> ifValid \case
                Left workerPreference -> render EditView {..}
                Right workerPreference -> do
                    workerPreference <- workerPreference |> updateRecord
                    setSuccessMessage "WorkerPreference updated"
                    redirectTo EditWorkerPreferenceAction {..}
    --
    action CreateWorkerPreferenceAction = do
        let workerPreference = newRecord @WorkerPreference
        workerPreference
            |> buildWorkerPreference
            |> ifValid \case
                Left workerPreference -> render NewView {..}
                Right workerPreference -> do
                    workerPreference <- workerPreference |> createRecord
                    setSuccessMessage "WorkerPreference created"
                    redirectTo WorkerPreferencesAction
    --
    action DeleteWorkerPreferenceAction {workerPreferenceId} = do
        workerPreference <- fetch workerPreferenceId
        deleteRecord workerPreference
        setSuccessMessage "WorkerPreference deleted"
        redirectTo WorkerPreferencesAction

buildWorkerPreference workerPreference =
    workerPreference
        |> fill @["personId", "sendDailyReminderAt"]
