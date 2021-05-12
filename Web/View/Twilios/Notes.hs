-- module Web.Controller.SmsMessages where

-- import Web.Controller.Prelude
-- import Web.View.SmsMessages.Index
-- import Web.View.SmsMessages.New
-- import Web.View.SmsMessages.Edit
-- import Web.View.SmsMessages.Show
-- import Network.HTTP.Req
-- import Data.Text.Encoding (encodeUtf8)
-- import qualified IHP.Log as Log

-- instance Controller SmsMessagesController where
--     action SmsMessagesAction = do
--         smsMessages <- query @SmsMessage |> fetch
--         render IndexView { .. }

--     action NewSmsMessageAction = do
--         let smsMessage = newRecord
--         render NewView { .. }

--     action ShowSmsMessageAction { smsMessageId } = do
--         smsMessage <- fetch smsMessageId
--         render ShowView { .. }

--     action EditSmsMessageAction { smsMessageId } = do
--         smsMessage <- fetch smsMessageId
--         render EditView { .. }

--     action UpdateSmsMessageAction { smsMessageId } = do
--         smsMessage <- fetch smsMessageId
--         smsMessage
--             |> buildSmsMessage
--             |> ifValid \case
--                 Left smsMessage -> render EditView { .. }
--                 Right smsMessage -> do
--                     smsMessage <- smsMessage |> updateRecord
--                     setSuccessMessage "SmsMessage updated"
--                     redirectTo EditSmsMessageAction { .. }

--     action CreateSmsMessageAction = do
--         Log.debug "YODER: sending text"
--         runReq defaultHttpConfig $ do
--             let payload = 
--                        "To" =: ("+18054035926" :: Text)
--                     <> "From" =: ("+12693593324" :: Text)
--                     <> "Body" =: ("Hello World!" :: Text)
--             resp <- post
--                 "71931aeda07720193eeae4f344e73ef6"
--                 (https "api.twilio.com" /: "2010-04-01" /: "Accounts" /: "AC828cf7fa609e74ef78861e56ad166f42" /: "Messages.json")
--                 (ReqBodyUrlEnc payload)
--             liftIO $ Log.debug $ "YODER: response: " <> show resp
--         let smsMessage = newRecord @SmsMessage
--         smsMessage
--             |> buildSmsMessage
--             |> ifValid \case
--                 Left smsMessfage -> render NewView { .. } 
--                 Right smsMessage -> do
--                     smsMessage <- smsMessage |> createRecord
--                     setSuccessMessage "SmsMessage created"
--                     redirectTo SmsMessagesAction

--     action DeleteSmsMessageAction { smsMessageId } = do
--         smsMessage <- fetch smsMessageId
--         deleteRecord smsMessage
--         setSuccessMessage "SmsMessage deleted"
--         redirectTo SmsMessagesAction

-- buildSmsMessage smsMessage = smsMessage
--     |> fill @'[]

-- post ::
--   MonadHttp m =>
--   HttpBody body =>
--   Text ->
--   Url 'Https ->
--   body ->
--   m BsResponse
-- post authToken url body =
--   req
--     POST
--     url
--     body
--     bsResponse
--     ( Network.HTTP.Req.basicAuth "AC828cf7fa609e74ef78861e56ad166f42" (encodeUtf8 authToken)
--     )
