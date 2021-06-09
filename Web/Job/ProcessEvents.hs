module Web.Job.ProcessEvents (initSingleton) where

import Application.Service.SendMessageAction
import Control.Concurrent (threadDelay)
import qualified Control.Exception
import Control.Monad.Loops (whileM_)
import IHP.Log as Log
import Web.Controller.Prelude

instance Job ProcessEventsJob where
    perform ProcessEventsJob {..} = do
        whileM_ (pure True) do
            Log.debug ("Running scheduled actions" :: Text)
            sendMessageActions <- fetchReadySendMessageActions
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
    SendMessageAction_ ->
    IO ()
runSendMessageAction sendMessageAction = do
    actionRunState <- fetch (get #actionRunStateId sendMessageAction)
    actionRunState |> set #state "running" |> updateRecord

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
            sendMessage sendMessageAction
        )
        ( do
            Log.error
                ( "Failed to run action (action_run_states.id = "
                    <> show (get #actionRunStateId sendMessageAction)
                    <> ")"
                )
            actionRunState <- fetch (get #actionRunStateId sendMessageAction)
            actionRunState |> set #state "failed" |> updateRecord
        )

    actionRunState <- fetch (get #actionRunStateId sendMessageAction)
    actionRunState |> set #state "finished" |> updateRecord
    pure ()

runInterval :: Int
runInterval = 60 * 1000000
