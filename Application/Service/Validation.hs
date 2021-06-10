module Application.Service.Validation (
    validateAndCreate,
    validateAndUpdate,
    ensureValid,
) where

import qualified Control.Exception as Exception
import Data.Text (unpack)
import IHP.Controller.Param (ifValid)
import IHP.ModelSupport
import qualified IHP.ModelSupport as ModelSupport
import IHP.Prelude hiding (toException)

newtype ValidationException = ValidationException {description :: Text} deriving (Show)

instance Exception ValidationException where
    displayException ValidationException {description} =
        unpack $ "Validations failed: " <> description

validateAndCreate ::
    ( ?modelContext :: ModelContext
    , HasField "meta" model ModelSupport.MetaBag
    , CanCreate model
    ) =>
    (model -> model) ->
    model ->
    IO model
validateAndCreate validate model = do
    validatedModel <- ensureValid validate model
    createRecord validatedModel

validateAndUpdate ::
    ( ?modelContext :: ModelContext
    , HasField "meta" model ModelSupport.MetaBag
    , CanUpdate model
    ) =>
    (model -> model) ->
    model ->
    IO model
validateAndUpdate validate model = do
    validatedModel <- ensureValid validate model
    updateRecord validatedModel

ensureValid ::
    (HasField "meta" model ModelSupport.MetaBag) =>
    (model -> model) ->
    model ->
    IO model
ensureValid validate model =
    model |> validate |> ifValid \case
        Left model -> throwIO $ toException model
        Right model -> pure model

toException :: (HasField "meta" model ModelSupport.MetaBag) => model -> ValidationException
toException model = ValidationException message
  where
    message = intercalate ", " messages
    messages = map (\(field, value) -> field <> "=" <> value) annotations
    annotations :: [(Text, Text)]
    annotations = getField @"annotations" meta
    meta :: ModelSupport.MetaBag
    meta = getField @"meta" model