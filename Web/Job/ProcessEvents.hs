module Web.Job.ProcessEvents where

import IHP.Log as Log
import Web.Controller.Prelude

instance Job ProcessEventsJob where
    perform ProcessEventsJob {..} = do
        Log.debug ("Hello World!" :: Text)
