module Application.Twilio.View where

import Application.Twilio.Query (EntityType (..), Row (..), Status (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Generated.Types as Types
import IHP.Prelude

data Message = Message
    { id :: !(Id Types.TwilioMessage)
    , fromPhoneNumber :: !Text
    , fromFirstName :: !Text
    , fromLastName :: !Text
    , toPhoneNumber :: !Text
    , toFirstName :: !Text
    , toLastName :: !Text
    , createdAt :: !UTCTime
    , status :: !Status
    , body :: !Text
    , entities :: ![Entity]
    }
    deriving (Eq, Show)

data Entity = Entity
    { entityType :: !EntityType
    , rawText :: !Text
    , confidence :: !Double
    }
    deriving (Eq, Show)

data EntityTuple = EntityTuple
    { entityType :: !EntityType
    , start :: !Int
    , end :: !Int
    , confidence :: !Double
    }
    deriving (Eq, Show)

buildMessages :: [Row] -> [Message]
buildMessages rows =
    case NonEmpty.nonEmpty rows of
        Just rows -> buildMessage <$> rowGroups rows
        Nothing -> []
  where
    rowGroups = NonEmpty.groupBy (\r1 r2 -> get #id r1 == get #id r2)

buildMessage :: NonEmpty Row -> Message
buildMessage rowGroup =
    let Row {..} = NonEmpty.head rowGroup
        body' = fromMaybe "<<missing body>>" body
        rows = NonEmpty.toList rowGroup
     in Message
            { id
            , fromPhoneNumber
            , fromFirstName
            , fromLastName
            , toPhoneNumber
            , toFirstName
            , toLastName
            , createdAt
            , status
            , body = body'
            , entities = buildEntities body' rows
            }

buildEntities :: Text -> [Row] -> [Entity]
buildEntities body rowGroup =
    buildEntity body <$> rowGroup |> entityTuples |> fillSpaces end
  where
    entityTuples = mapMaybe entityTuple
    end = Text.length body

buildEntity :: Text -> EntityTuple -> Entity
buildEntity body EntityTuple {..} =
    Entity
        { entityType
        , rawText = slice start end body
        , confidence
        }

entityTuple :: Row -> Maybe EntityTuple
entityTuple Row {..} =
    case (entityStart, entityEnd, entityType, entityConfidence) of
        (Just entityStart, Just entityEnd, Just entityType, Just entityConfidence) ->
            Just
                EntityTuple
                    { entityType
                    , start = entityStart
                    , end = entityEnd
                    , confidence = entityConfidence
                    }
        _ -> Nothing

fillSpaces :: Int -> [EntityTuple] -> [EntityTuple]
fillSpaces end tuples =
    tuples
        |> fillSpacesBetween
        |> fillSpaceBefore end
        |> fillSpaceAfter end

fillSpacesBetween :: [EntityTuple] -> [EntityTuple]
fillSpacesBetween (tuple1 : tuple2 : tuples)
    | spaceBetween = tuple1 : middleTuple : fillSpacesBetween (tuple2 : tuples)
    | otherwise = tuple1 : fillSpacesBetween (tuple2 : tuples)
  where
    spaceBetween = get #end tuple1 /= get #start tuple2
    middleTuple = tupleBetween tuple1 tuple2
    tupleBetween tuple1 tuple2 = unrecognizedTuple (get #end tuple1) (get #start tuple2)
fillSpacesBetween [tuple] = [tuple]
fillSpacesBetween [] = []

fillSpaceBefore :: Int -> [EntityTuple] -> [EntityTuple]
fillSpaceBefore end [] = [unrecognizedTuple 0 end]
fillSpaceBefore end (tuple : tuples)
    | spaceBefore = tupleBefore : tuple : tuples
    | otherwise = tuple : tuples
  where
    spaceBefore = get #start tuple /= 0
    tupleBefore = unrecognizedTuple 0 (get #start tuple)

fillSpaceAfter :: Int -> [EntityTuple] -> [EntityTuple]
fillSpaceAfter end [] = [unrecognizedTuple 0 end]
fillSpaceAfter end tuples = case last tuples of
    Just tuple -> if spaceAfter tuple then tuples <> [tupleAfter tuple] else tuples
    Nothing -> fillSpaceAfter end []
  where
    spaceAfter tuple = get #end tuple /= end
    tupleAfter tuple = unrecognizedTuple (get #end tuple) end

unrecognizedTuple :: Int -> Int -> EntityTuple
unrecognizedTuple start end =
    EntityTuple
        { entityType = Unrecognized
        , start
        , end
        , confidence = 1.0
        }

slice :: Int -> Int -> Text -> Text
slice start end = Text.take (end - start) . Text.drop start
