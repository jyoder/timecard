module Application.Base.WorkerSettings where

import Generated.Types
import IHP.ControllerPrelude

data Language
    = English
    | Spanish
    deriving (Eq, Show)

fetchPreferredLanguageForWorker ::
    (?modelContext :: ModelContext) =>
    Id Person ->
    IO Language
fetchPreferredLanguageForWorker workerId = do
    workerSettings <-
        query @WorkerSetting
            |> filterWhere (#personId, workerId)
            |> fetchOne
    pure $ toLanguage $ get #preferredLanguage workerSettings

toLanguage :: Text -> Language
toLanguage text
    | text == english = English
    | text == spanish = Spanish
    | otherwise = English

english :: Text
english = "english"

spanish :: Text
spanish = "spanish"