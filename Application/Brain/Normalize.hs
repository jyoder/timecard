module Application.Brain.Normalize where

import Data.Text (unpack)
import Data.Text.Internal.Search (indices)
import qualified Data.Text.Read
import IHP.Prelude
import qualified Text.Read
import Text.Regex.TDFA

clockedInAt :: Text -> Maybe TimeOfDay
clockedInAt text
    | text =~ oneDigitRegex = oneDigitClockIn (text =~ oneDigitRegex :: Text)
    | text =~ twoDigitsRegex = twoDigits (text =~ twoDigitsRegex :: Text)
    | text =~ threeDigitsRegex = threeDigitsClockIn (text =~ threeDigitsRegex :: (Text, Text, Text, [Text]))
    | text =~ fourDigitsRegex = fourDigits (text =~ fourDigitsRegex :: (Text, Text, Text, [Text]))
    | text =~ oneDigitWithAmRegex = oneDigitWithAm (text =~ oneDigitWithAmRegex :: (Text, Text, Text, [Text]))
    | text =~ oneDigitWithPmRegex = oneDigitWithPm (text =~ oneDigitWithPmRegex :: (Text, Text, Text, [Text]))
    | text =~ digitsWithAmRegex = digitsWithAm (text =~ digitsWithAmRegex :: (Text, Text, Text, [Text]))
    | text =~ digitsWithPmRegex = digitsWithPm (text =~ digitsWithPmRegex :: (Text, Text, Text, [Text]))
    | otherwise = Nothing

clockedOutAt :: Text -> Maybe TimeOfDay
clockedOutAt text
    | text =~ oneDigitRegex = oneDigitClockOut (text =~ oneDigitRegex :: Text)
    | text =~ twoDigitsRegex = twoDigits (text =~ twoDigitsRegex :: Text)
    | text =~ threeDigitsRegex = threeDigitsClockOut (text =~ threeDigitsRegex :: (Text, Text, Text, [Text]))
    | text =~ fourDigitsRegex = fourDigits (text =~ fourDigitsRegex :: (Text, Text, Text, [Text]))
    | text =~ oneDigitWithAmRegex = oneDigitWithAm (text =~ oneDigitWithAmRegex :: (Text, Text, Text, [Text]))
    | text =~ oneDigitWithPmRegex = oneDigitWithPm (text =~ oneDigitWithPmRegex :: (Text, Text, Text, [Text]))
    | text =~ digitsWithAmRegex = digitsWithAm (text =~ digitsWithAmRegex :: (Text, Text, Text, [Text]))
    | text =~ digitsWithPmRegex = digitsWithPm (text =~ digitsWithPmRegex :: (Text, Text, Text, [Text]))
    | otherwise = Nothing

oneDigitClockIn :: Text -> Maybe TimeOfDay
oneDigitClockIn text =
    case text of
        "1" -> Just $ TimeOfDay 13 0 0
        "2" -> Just $ TimeOfDay 14 0 0
        "3" -> Just $ TimeOfDay 15 0 0
        "4" -> Just $ TimeOfDay 16 0 0
        "5" -> Just $ TimeOfDay 17 0 0
        "6" -> Just $ TimeOfDay 6 0 0
        "7" -> Just $ TimeOfDay 7 0 0
        "8" -> Just $ TimeOfDay 8 0 0
        "9" -> Just $ TimeOfDay 9 0 0
        _ -> Nothing

oneDigitClockOut :: Text -> Maybe TimeOfDay
oneDigitClockOut text =
    case text of
        "1" -> Just $ TimeOfDay 13 0 0
        "2" -> Just $ TimeOfDay 14 0 0
        "3" -> Just $ TimeOfDay 15 0 0
        "4" -> Just $ TimeOfDay 16 0 0
        "5" -> Just $ TimeOfDay 17 0 0
        "6" -> Just $ TimeOfDay 18 0 0
        "7" -> Just $ TimeOfDay 19 0 0
        "8" -> Just $ TimeOfDay 20 0 0
        "9" -> Just $ TimeOfDay 21 0 0
        _ -> Nothing

twoDigits :: Text -> Maybe TimeOfDay
twoDigits text =
    case text of
        "10" -> Just $ TimeOfDay 10 0 0
        "11" -> Just $ TimeOfDay 11 0 0
        "12" -> Just $ TimeOfDay 12 0 0
        "13" -> Just $ TimeOfDay 13 0 0
        "14" -> Just $ TimeOfDay 14 0 0
        "15" -> Just $ TimeOfDay 15 0 0
        "16" -> Just $ TimeOfDay 16 0 0
        "17" -> Just $ TimeOfDay 17 0 0
        "18" -> Just $ TimeOfDay 18 0 0
        "19" -> Just $ TimeOfDay 19 0 0
        "20" -> Just $ TimeOfDay 20 0 0
        "21" -> Just $ TimeOfDay 21 0 0
        "22" -> Just $ TimeOfDay 22 0 0
        "23" -> Just $ TimeOfDay 23 0 0
        _ -> Nothing

threeDigitsClockIn :: (Text, Text, Text, [Text]) -> Maybe TimeOfDay
threeDigitsClockIn (_, _, _, groups) =
    case (groups, minutes) of
        (["1", _], Just minutes) -> Just $ TimeOfDay 13 minutes 0
        (["2", _], Just minutes) -> Just $ TimeOfDay 14 minutes 0
        (["3", _], Just minutes) -> Just $ TimeOfDay 15 minutes 0
        (["4", _], Just minutes) -> Just $ TimeOfDay 16 minutes 0
        (["5", _], Just minutes) -> Just $ TimeOfDay 17 minutes 0
        (["6", _], Just minutes) -> Just $ TimeOfDay 6 minutes 0
        (["7", _], Just minutes) -> Just $ TimeOfDay 7 minutes 0
        (["8", _], Just minutes) -> Just $ TimeOfDay 8 minutes 0
        (["9", _], Just minutes) -> Just $ TimeOfDay 9 minutes 0
        _ -> Nothing
  where
    minutes = case groups of
        [_, minutesText] -> Just (Text.Read.read (unpack minutesText) :: Int)
        _ -> Nothing

threeDigitsClockOut :: (Text, Text, Text, [Text]) -> Maybe TimeOfDay
threeDigitsClockOut (_, _, _, groups) =
    case (groups, minutes) of
        (["1", _], Just minutes) -> Just $ TimeOfDay 13 minutes 0
        (["2", _], Just minutes) -> Just $ TimeOfDay 14 minutes 0
        (["3", _], Just minutes) -> Just $ TimeOfDay 15 minutes 0
        (["4", _], Just minutes) -> Just $ TimeOfDay 16 minutes 0
        (["5", _], Just minutes) -> Just $ TimeOfDay 17 minutes 0
        (["6", _], Just minutes) -> Just $ TimeOfDay 18 minutes 0
        (["7", _], Just minutes) -> Just $ TimeOfDay 19 minutes 0
        (["8", _], Just minutes) -> Just $ TimeOfDay 20 minutes 0
        (["9", _], Just minutes) -> Just $ TimeOfDay 21 minutes 0
        _ -> Nothing
  where
    minutes = case groups of
        [_, minutesText] -> Just (Text.Read.read (unpack minutesText) :: Int)
        _ -> Nothing

fourDigits :: (Text, Text, Text, [Text]) -> Maybe TimeOfDay
fourDigits (_, _, _, groups) =
    case groups of
        [hourText, minutesText] ->
            let hour = Text.Read.read (unpack hourText) :: Int
                minute = Text.Read.read (unpack minutesText) :: Int
             in if hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59
                    then Just $ TimeOfDay hour minute 0
                    else Nothing
        _ -> Nothing

oneDigitWithAm :: (Text, Text, Text, [Text]) -> Maybe TimeOfDay
oneDigitWithAm (_, _, _, groups) =
    case groups of
        ["1"] -> Just $ TimeOfDay 1 0 0
        ["2"] -> Just $ TimeOfDay 2 0 0
        ["3"] -> Just $ TimeOfDay 3 0 0
        ["4"] -> Just $ TimeOfDay 4 0 0
        ["5"] -> Just $ TimeOfDay 5 0 0
        ["6"] -> Just $ TimeOfDay 6 0 0
        ["7"] -> Just $ TimeOfDay 7 0 0
        ["8"] -> Just $ TimeOfDay 8 0 0
        ["9"] -> Just $ TimeOfDay 9 0 0
        ["10"] -> Just $ TimeOfDay 10 0 0
        ["11"] -> Just $ TimeOfDay 11 0 0
        ["12"] -> Just $ TimeOfDay 0 0 0
        _ -> Nothing

oneDigitWithPm :: (Text, Text, Text, [Text]) -> Maybe TimeOfDay
oneDigitWithPm (_, _, _, groups) =
    case groups of
        ["1"] -> Just $ TimeOfDay 13 0 0
        ["2"] -> Just $ TimeOfDay 14 0 0
        ["3"] -> Just $ TimeOfDay 15 0 0
        ["4"] -> Just $ TimeOfDay 16 0 0
        ["5"] -> Just $ TimeOfDay 17 0 0
        ["6"] -> Just $ TimeOfDay 18 0 0
        ["7"] -> Just $ TimeOfDay 19 0 0
        ["8"] -> Just $ TimeOfDay 20 0 0
        ["9"] -> Just $ TimeOfDay 21 0 0
        ["10"] -> Just $ TimeOfDay 22 0 0
        ["11"] -> Just $ TimeOfDay 23 0 0
        ["12"] -> Just $ TimeOfDay 12 0 0
        _ -> Nothing

digitsWithAm :: (Text, Text, Text, [Text]) -> Maybe TimeOfDay
digitsWithAm (_, _, _, groups) =
    case (groups, minutes) of
        (["1", _], Just minutes) -> Just $ TimeOfDay 1 minutes 0
        (["2", _], Just minutes) -> Just $ TimeOfDay 2 minutes 0
        (["3", _], Just minutes) -> Just $ TimeOfDay 3 minutes 0
        (["4", _], Just minutes) -> Just $ TimeOfDay 4 minutes 0
        (["5", _], Just minutes) -> Just $ TimeOfDay 5 minutes 0
        (["6", _], Just minutes) -> Just $ TimeOfDay 6 minutes 0
        (["7", _], Just minutes) -> Just $ TimeOfDay 7 minutes 0
        (["8", _], Just minutes) -> Just $ TimeOfDay 8 minutes 0
        (["9", _], Just minutes) -> Just $ TimeOfDay 9 minutes 0
        (["10", _], Just minutes) -> Just $ TimeOfDay 10 minutes 0
        (["11", _], Just minutes) -> Just $ TimeOfDay 11 minutes 0
        (["12", _], Just minutes) -> Just $ TimeOfDay 0 minutes 0
        _ -> Nothing
  where
    minutes = case groups of
        [_, minutesText] -> Just (Text.Read.read (unpack minutesText) :: Int)
        _ -> Nothing

digitsWithPm :: (Text, Text, Text, [Text]) -> Maybe TimeOfDay
digitsWithPm (_, _, _, groups) =
    case (groups, minutes) of
        (["1", _], Just minutes) -> Just $ TimeOfDay 13 minutes 0
        (["2", _], Just minutes) -> Just $ TimeOfDay 14 minutes 0
        (["3", _], Just minutes) -> Just $ TimeOfDay 15 minutes 0
        (["4", _], Just minutes) -> Just $ TimeOfDay 16 minutes 0
        (["5", _], Just minutes) -> Just $ TimeOfDay 17 minutes 0
        (["6", _], Just minutes) -> Just $ TimeOfDay 18 minutes 0
        (["7", _], Just minutes) -> Just $ TimeOfDay 19 minutes 0
        (["8", _], Just minutes) -> Just $ TimeOfDay 20 minutes 0
        (["9", _], Just minutes) -> Just $ TimeOfDay 21 minutes 0
        (["10", _], Just minutes) -> Just $ TimeOfDay 22 minutes 0
        (["11", _], Just minutes) -> Just $ TimeOfDay 23 minutes 0
        (["12", _], Just minutes) -> Just $ TimeOfDay 12 minutes 0
        _ -> Nothing
  where
    minutes = case groups of
        [_, minutesText] -> Just (Text.Read.read (unpack minutesText) :: Int)
        _ -> Nothing

oneDigitRegex :: Text
oneDigitRegex = "\\`[0-9]\\'"

twoDigitsRegex :: Text
twoDigitsRegex = "\\`[0-9][0-9]\\'"

threeDigitsRegex :: Text
threeDigitsRegex = "\\`([0-9]):?([0-9][0-9])\\'"

fourDigitsRegex :: Text
fourDigitsRegex = "\\`([0-9][0-9]):?([0-9][0-9])\\'"

oneDigitWithAmRegex :: Text
oneDigitWithAmRegex = "\\`([0-9]+) ?[aA][mM]?\\'"

oneDigitWithPmRegex :: Text
oneDigitWithPmRegex = "\\`([0-9]+) ?[pP][mM]?\\'"

digitsWithAmRegex :: Text
digitsWithAmRegex = "\\`([0-9][0-9]?):?([0-9][0-9]) ?[aA][mM]?\\'"

digitsWithPmRegex :: Text
digitsWithPmRegex = "\\`([0-9][0-9]?):?([0-9][0-9]) ?[pP][mM]?\\'"

hoursWorked :: Text -> Maybe Double
hoursWorked text
    | text =~ hoursWorkedRegex = hours (text =~ hoursWorkedRegex :: (Text, Text, Text, [Text]))
    | text =~ hoursWorkedWithHalfHourRegex =
        hoursWithHalfHour (text =~ hoursWorkedWithHalfHourRegex :: (Text, Text, Text, [Text]))
    | otherwise = Nothing

hours :: (Text, Text, Text, [Text]) -> Maybe Double
hours (_, _, _, groups) =
    case groups of
        text : _ -> case Data.Text.Read.double text of
            Left _ -> Nothing
            Right (hours, _) -> Just hours
        _ -> Nothing

hoursWithHalfHour :: (Text, Text, Text, [Text]) -> Maybe Double
hoursWithHalfHour (_, _, _, groups) =
    case groups of
        text : _ -> case Data.Text.Read.double text of
            Left _ -> Nothing
            Right (hours, _) -> Just $ hours + 0.5
        _ -> Nothing

hoursWorkedRegex :: Text
hoursWorkedRegex = "\\`([0-9]+\\.?[0-9]*) *((h)|(hr)|(hrs)|(hours))?\\'"

hoursWorkedWithHalfHourRegex :: Text
hoursWorkedWithHalfHourRegex = "\\`([0-9]+) *(1/2) *((h)|(hr)|(hrs)|(hours))?\\'"

jobName :: Maybe Text -> Maybe Text -> Maybe Text
jobName (Just previous) Nothing = Just previous
jobName Nothing (Just current) = Just current
jobName (Just previous) (Just current) =
    if isSubstring (toLower current) (toLower previous)
        then Just previous
        else Just current
jobName _ _ = Nothing

isSubstring :: Text -> Text -> Bool
isSubstring needle haystack =
    case indices needle haystack of
        (_ : _) -> True
        _ -> False
