module Web.View.Timecards.Status (
    TimecardStatus (..),
    timecardStatus,
    timecardStatusClasses,
    timecardStatusLabel,
) where

import qualified Application.Timecard.View as V
import IHP.Prelude

data TimecardStatus = TimecardStatus
    { statusClasses :: !Text
    , statusLabel :: !Text
    }
    deriving (Eq, Show)

timecardStatus :: V.Status -> TimecardStatus
timecardStatus status =
    TimecardStatus
        { statusClasses = timecardStatusClasses status
        , statusLabel = timecardStatusLabel status
        }

timecardStatusClasses :: V.Status -> Text
timecardStatusClasses =
    \case
        V.TimecardInProgress -> "badge badge-pill badge-secondary"
        V.TimecardReadyForReview -> "badge badge-pill badge-primary"
        V.TimecardUnderReview _ -> "badge badge-pill badge-primary"
        V.TimecardSigned _ -> "badge badge-pill badge-success"

timecardStatusLabel :: V.Status -> Text
timecardStatusLabel =
    \case
        V.TimecardInProgress -> "In Progress"
        V.TimecardReadyForReview -> "Ready For Review"
        V.TimecardUnderReview _ -> "Under Review"
        V.TimecardSigned _ -> "Signed"