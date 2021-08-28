module Config where

import qualified Application.Config.Config as Config
import IHP.Environment
import IHP.FrameworkConfig
import qualified IHP.Log as Log
import IHP.Log.Types
import IHP.Prelude

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    logger <-
        liftIO $
            newLogger
                def
                    { level = Debug
                    , formatter = withTimeFormatter
                    }
    option logger

    config <- liftIO Config.load
    let Config.T {..} = config

    option twilioConfig
    option vertexAiConfig
