module Application.Twilio.TwilioMessageEntity (
    validate,
) where

import Generated.Types
import IHP.Prelude

validate :: TwilioMessageEntity -> TwilioMessageEntity
validate twilioMessageEntity = twilioMessageEntity