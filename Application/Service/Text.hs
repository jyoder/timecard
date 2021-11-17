module Application.Service.Text (
    isBlank,
) where

import Data.Text (strip)
import IHP.Prelude

isBlank :: Text -> Bool
isBlank text = strip text == ""