module Application.Config.Environment (
    loadEnvVars,
    findVar,
) where

import Data.Text (pack, unpack)
import IHP.Prelude
import System.Environment (lookupEnv)

loadEnvVars :: [Text] -> IO (Either Text [(Text, Text)])
loadEnvVars names = do
    results <- mapM loadEnvVar names
    let errors = lefts results
    if null errors
        then pure $ Right $ rights results
        else pure $ Left $ "Error: Missing environment variables: " <> intercalate ", " errors

loadEnvVar :: Text -> IO (Either Text (Text, Text))
loadEnvVar name =
    lookupEnv (unpack name)
        >>= \case
            Just value -> pure $ Right (name, pack value)
            Nothing -> pure $ Left name

findVar :: [(Text, Text)] -> Text -> Text
findVar vars name =
    let value = find (\(name', value) -> name' == name) vars
     in maybe "" snd value
