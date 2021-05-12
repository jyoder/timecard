module Web.Controller.Communications where

import Web.Controller.Prelude
import Web.View.Communications.Index

instance Controller CommunicationsController where
    action CommunicationsAction = do
        render IndexView
