module Web.Job.ProcessEvents (initSingleton) where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import Application.Service.Validation (validateAndUpdate)
import Control.Concurrent (threadDelay)
import qualified Control.Exception
import Control.Monad.Loops (whileM_)
import IHP.Log as Log
import Web.Controller.Prelude

instance Job ProcessEventsJob where
    perform ProcessEventsJob {..} = do
        whileM_ (pure True) do
            Log.debug ("Running scheduled actions" :: Text)
            now <- getCurrentTime
            sendMessageActions <- SendMessageAction.fetchReady now
            mapM_ runSendMessageAction sendMessageActions
            threadDelay runInterval

initSingleton :: ConfigBuilder -> IO ()
initSingleton configBuilder = do
    frameworkConfig <- buildFrameworkConfig configBuilder
    modelContext <- initModelContext frameworkConfig
    let ?modelContext = modelContext

    processEventsJobs <- query @ProcessEventsJob |> fetch
    deleteRecords processEventsJobs
    newRecord @ProcessEventsJob |> create

    pure ()

initModelContext :: FrameworkConfig -> IO ModelContext
initModelContext FrameworkConfig {..} = do
    createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger

runSendMessageAction ::
    (?modelContext :: ModelContext, ?context :: FrameworkConfig) =>
    SendMessageAction.T ->
    IO ()
runSendMessageAction sendMessageAction = do
    actionRunState <- fetch (get #actionRunStateId sendMessageAction)
    ActionRunState.updateRunning actionRunState

    Control.Exception.onException
        ( do
            Log.info
                ( "Sending message from "
                    <> get #fromNumber sendMessageAction
                    <> " to "
                    <> get #toNumber sendMessageAction
                    <> ": "
                    <> get #body sendMessageAction
                )
            SendMessageAction.perform sendMessageAction
        )
        ( do
            Log.error
                ( "Failed to run action (action_run_states.id = "
                    <> show (get #actionRunStateId sendMessageAction)
                    <> ")"
                )
            actionRunState <- fetch (get #actionRunStateId sendMessageAction)
            ActionRunState.updateFailed actionRunState
        )

    actionRunState <- fetch (get #actionRunStateId sendMessageAction)
    ActionRunState.updateFinished actionRunState
    pure ()

runInterval :: Int
runInterval = 60 * 1000000
