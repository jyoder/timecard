module Application.Service.Time (parseDay) where

import Data.Text (unpack)
import IHP.Prelude

parseDay :: Text -> Maybe Day
parseDay dayString = parseTimeM True defaultTimeLocale "%Y-%m-%d" $ unpack dayString