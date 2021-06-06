module Web.Job.ProcessEvents (initSingleton) where

import Control.Concurrent (threadDelay)
import Control.Monad.Loops (whileM_)
import IHP.Log as Log
import Web.Controller.Prelude

instance Job ProcessEventsJob where
    perform ProcessEventsJob {..} = do
        whileM_ (pure True) do
            Log.debug ("Processing events" :: Text)
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

runInterval :: Int
runInterval = 60 * 1000000