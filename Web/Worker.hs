module Web.Worker where

import IHP.Prelude
import Web.Types
import Generated.Types
import IHP.Job.Runner
import IHP.Job.Types

import Web.Job.ProcessEvents
import Web.Job.FetchEntityPrediction

instance Worker WebApplication where
    workers _ =
        [ worker @ProcessEventsJob
        -- Generator Marker
        , worker @FetchEntityPredictionJob
        ]
