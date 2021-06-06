module Main where

import IHP.Prelude

import Config
import IHP.FrameworkConfig
import IHP.Job.Types
import IHP.RouterSupport
import qualified IHP.Server
import Web.FrontController
import qualified Web.Job.ProcessEvents
import Web.Types
import Web.Worker

instance FrontController RootApplication where
    controllers =
        [ mountFrontController WebApplication
        ]

instance Worker RootApplication where
    workers _ = workers WebApplication

main :: IO ()
main = do
    Web.Job.ProcessEvents.initSingleton config
    IHP.Server.run config
