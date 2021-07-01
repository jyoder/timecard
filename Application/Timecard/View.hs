module Application.Timecard.View (
    Timecard (..),
    Status (..),
    TimecardEntry (..),
    AccessToken (..),
    buildTimecards,
    buildTimecard,
) where

import Application.Timecard.Query (Row (..))
import Control.Exception.Safe (throwString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Generated.Types as Types
import IHP.Prelude

data Timecard = Timecard
    { id :: !(Id Types.Timecard)
    , personId :: !(Id Types.Person)
    , weekOf :: !Day
    , status :: !Status
    , entries :: ![TimecardEntry]
    }

data Status
    = TimecardInProgress
    | TimecardReadyForReview
    | TimecardUnderReview !AccessToken
    | TimecardSigned !Signing

data TimecardEntry = TimecardEntry
    { id :: !(Id Types.TimecardEntry)
    , date :: !Day
    , jobName :: !Text
    , hoursWorked :: !Double
    , workDone :: !Text
    , invoiceTranslation :: !Text
    }

data AccessToken = AccessToken
    { id :: !(Id Types.AccessToken)
    , value :: !Text
    , expiresAt :: !UTCTime
    , isRevoked :: !Bool
    }

data Signing = Signing
    { id :: !(Id Types.Signing)
    , signedAt :: !UTCTime
    }

buildTimecards :: [Row] -> [Timecard]
buildTimecards queryRows =
    (buildTimecard <$> groupBy compareRows queryRows) |> catMaybes
  where
    compareRows row1 row2 = get #timecardId row1 == get #timecardId row2

buildTimecard :: [Row] -> Maybe Timecard
buildTimecard [] = Nothing
buildTimecard (firstRow : otherRows) =
    Just
        Timecard
            { id = get #timecardId firstRow
            , personId = get #timecardPersonId firstRow
            , weekOf = get #timecardWeekOf firstRow
            , status = buildStatus (firstRow :| otherRows)
            , entries = buildTimecardEntry <$> firstRow : otherRows
            }

buildStatus :: NonEmpty Row -> Status
buildStatus rows =
    let (id, value, expiresAt, isRevoked, signingId, signedAt) =
            ( get #accessTokenId firstRow
            , get #accessTokenValue firstRow
            , get #accessTokenExpiresAt firstRow
            , get #accessTokenIsRevoked firstRow
            , get #signingId firstRow
            , get #signingSignedAt firstRow
            )
     in case (id, value, expiresAt, isRevoked, signingId, signedAt) of
            (Just id, Just value, Just expiresAt, Just isRevoked, Nothing, Nothing) ->
                TimecardUnderReview $ AccessToken {..}
            (_, _, _, _, Just signingId, Just signedAt) ->
                TimecardSigned $ Signing signingId signedAt
            _ ->
                if isTimecardComplete rows
                    then TimecardReadyForReview
                    else TimecardInProgress
  where
    firstRow = NE.head rows
    otherRows = NE.tail rows

buildTimecardEntry :: Row -> TimecardEntry
buildTimecardEntry row =
    TimecardEntry
        { id = get #timecardEntryId row
        , date = get #timecardEntryDate row
        , jobName = get #timecardEntryJobName row
        , hoursWorked = get #timecardEntryHoursWorked row
        , workDone = get #timecardEntryWorkDone row
        , invoiceTranslation = get #timecardEntryInvoiceTranslation row
        }

isTimecardComplete :: NonEmpty Row -> Bool
isTimecardComplete rows =
    startsWithMonday uniqueSortedDays
        && daysAreConsecutive uniqueSortedDays
        && length uniqueSortedDays == daysInWorkWeek
  where
    uniqueSortedDays = days |> NE.sort |> NE.nub
    days = get #timecardEntryDate <$> rows
    daysInWorkWeek = 5

startsWithMonday :: NonEmpty Day -> Bool
startsWithMonday uniqueSortedDays =
    let (_, _, dayOfWeek) = toWeekDate $ NE.head uniqueSortedDays
     in dayOfWeek == 1

daysAreConsecutive :: NonEmpty Day -> Bool
daysAreConsecutive uniqueSortedDays =
    all (uncurry (==)) (zip [firstDay ..] (NE.toList uniqueSortedDays))
  where
    firstDay = NE.head uniqueSortedDays
