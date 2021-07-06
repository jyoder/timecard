module Application.Twilio.View (
    TwilioMessage (..),
    Row (..),
    Status (..),
) where

import Application.Twilio.Query (Row (..), Status (..))

type TwilioMessage = Row
