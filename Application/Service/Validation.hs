module Application.Service.Validation (
    nonBlank,
    validateAndCreate,
    validateAndUpdate,
    ensureValid,
    validateAndCreateIO,
    validateAndUpdateIO,
    ensureValidIO,
    ValidationException (..),
) where

import qualified Control.Exception as Exception
import Data.Text (strip, unpack)
import IHP.Controller.Param (ifValid)
import IHP.ModelSupport
import qualified IHP.ModelSupport as ModelSupport
import IHP.Prelude hiding (toException)
import IHP.ValidationSupport.Types (ValidatorResult (..))

newtype ValidationException = ValidationException
    { description :: Text
    }
    deriving (Show, Eq)

instance Exception ValidationException where
    displayException ValidationException {description} =
        unpack $ "Validations failed: " <> description

nonBlank :: Text -> ValidatorResult
nonBlank text = case strip text of
    "" -> Failure "This field cannot be blank"
    _ -> Success

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

validateAndCreateIO ::
    ( ?modelContext :: ModelContext
    , HasField "meta" model ModelSupport.MetaBag
    , CanCreate model
    ) =>
    (model -> IO model) ->
    model ->
    IO model
validateAndCreateIO validate model = do
    validatedModel <- ensureValidIO validate model
    createRecord validatedModel

validateAndUpdateIO ::
    ( ?modelContext :: ModelContext
    , HasField "meta" model ModelSupport.MetaBag
    , CanUpdate model
    ) =>
    (model -> IO model) ->
    model ->
    IO model
validateAndUpdateIO validate model = do
    validatedModel <- ensureValidIO validate model
    updateRecord validatedModel

ensureValidIO ::
    (HasField "meta" model ModelSupport.MetaBag) =>
    (model -> IO model) ->
    model ->
    IO model
ensureValidIO validate model = do
    model |> validate >>= ifValid \case
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
