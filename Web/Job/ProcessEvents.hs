module Web.Job.ProcessEvents (initSingleton) where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.SendMessageAction as SendMessageAction
import Application.Service.Validation (validateAndUpdate)
import Control.Concurrent (threadDelay)
import qualified Control.Exception
import Control.Monad.Loops (whileM_)
import qualified IHP.Log as Log
import Web.Controller.Prelude

instance Job ProcessEventsJob where
    perform ProcessEventsJob {..} = do
        whileM_ (pure True) do
            Log.info ("Running scheduled actions" :: Text)

            now <- getCurrentTime
            updateTimestamp id now

            sendMessageActions <- SendMessageAction.fetchReadyToRun now
            mapM_ runSendMessageAction sendMessageActions

            threadDelay runInterval

initSingleton :: ConfigBuilder -> IO ()
initSingleton configBuilder = do
    frameworkConfig <- buildFrameworkConfig configBuilder
    modelContext <- initModelContext frameworkConfig
    let ?modelContext = modelContext

    processEventsJobs <- query @ProcessEventsJob |> fetch
    deleteRecords processEventsJobs

    runAt <- addUTCTime startupDelay <$> getCurrentTime
    newRecord @ProcessEventsJob |> set #runAt runAt |> createRecord

    pure ()

initModelContext :: FrameworkConfig -> IO ModelContext
initModelContext FrameworkConfig {..} = do
    createModelContext dbPoolIdleTime dbPoolMaxConnections databaseUrl logger

updateTimestamp :: (?modelContext :: ModelContext) => Id ProcessEventsJob -> UTCTime -> IO ()
updateTimestamp processEventsJobId now = do
    processEventsJob <- fetch processEventsJobId
    processEventsJob |> set #updatedAt now |> updateRecord
    pure ()

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

startupDelay :: NominalDiffTime
startupDelay = secondsToNominalDiffTime (60 * 2)

runInterval :: Int
runInterval = 60 * 1000000
