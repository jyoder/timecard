module Web.Controller.WorkerSettings where

import Web.Controller.Prelude
import Web.View.WorkerSettings.Edit
import Web.View.WorkerSettings.Index
import Web.View.WorkerSettings.New
import Web.View.WorkerSettings.Show

instance Controller WorkerSettingsController where
    beforeAction = ensureIsUser

    action WorkerSettingsAction = do
        workerSettings <- query @WorkerSetting |> fetch
        render IndexView {..}
    --
    action NewWorkerSettingAction = do
        let workerSetting = newRecord
        render NewView {..}
    --
    action ShowWorkerSettingAction {workerSettingId} = do
        workerSetting <- fetch workerSettingId
        render ShowView {..}
    --
    action EditWorkerSettingAction {workerSettingId} = do
        workerSetting <- fetch workerSettingId
        render EditView {..}
    --
    action UpdateWorkerSettingAction {workerSettingId} = do
        workerSetting <- fetch workerSettingId
        workerSetting
            |> buildWorkerSetting
            |> ifValid \case
                Left workerSetting -> render EditView {..}
                Right workerSetting -> do
                    workerSetting <- workerSetting |> updateRecord
                    setSuccessMessage "WorkerSetting updated"
                    redirectTo EditWorkerSettingAction {..}
    --
    action CreateWorkerSettingAction = do
        let workerSetting = newRecord @WorkerSetting
        workerSetting
            |> buildWorkerSetting
            |> ifValid \case
                Left workerSetting -> render NewView {..}
                Right workerSetting -> do
                    workerSetting <- workerSetting |> createRecord
                    setSuccessMessage "WorkerSetting created"
                    redirectTo WorkerSettingsAction
    --
    action DeleteWorkerSettingAction {workerSettingId} = do
        workerSetting <- fetch workerSettingId
        deleteRecord workerSetting
        setSuccessMessage "WorkerSetting deleted"
        redirectTo WorkerSettingsAction

buildWorkerSetting workerSetting =
    workerSetting
        |> fill @["personId", "isActive", "sendDailyReminderAt", "preferredLanguage"]
