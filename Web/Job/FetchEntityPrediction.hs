module Web.Job.FetchEntityPrediction where

import Web.Controller.Prelude

instance Job FetchEntityPredictionJob where
    perform FetchEntityPredictionJob {..} = do
        putStrLn "Hello World!"
