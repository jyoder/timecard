module Web.Controller.PhoneMessages where

import Web.Controller.Prelude
import Web.View.PhoneMessages.Index
import Web.View.PhoneMessages.New
import Web.View.PhoneMessages.Edit
import Web.View.PhoneMessages.Show

instance Controller PhoneMessagesController where
    action PhoneMessagesAction = do
        phoneMessages <- query @PhoneMessage |> fetch
        render IndexView { .. }

    action NewPhoneMessageAction = do
        let phoneMessage = newRecord
        render NewView { .. }

    action ShowPhoneMessageAction { phoneMessageId } = do
        phoneMessage <- fetch phoneMessageId
        render ShowView { .. }

    action EditPhoneMessageAction { phoneMessageId } = do
        phoneMessage <- fetch phoneMessageId
        render EditView { .. }

    action UpdatePhoneMessageAction { phoneMessageId } = do
        phoneMessage <- fetch phoneMessageId
        phoneMessage
            |> buildPhoneMessage
            |> ifValid \case
                Left phoneMessage -> render EditView { .. }
                Right phoneMessage -> do
                    phoneMessage <- phoneMessage |> updateRecord
                    setSuccessMessage "Phone Message updated"
                    redirectTo EditPhoneMessageAction { .. }

    action CreatePhoneMessageAction = do
        let phoneMessage = newRecord @PhoneMessage
        phoneMessage
            |> buildPhoneMessage
            |> ifValid \case
                Left phoneMessage -> render NewView { .. } 
                Right phoneMessage -> do
                    phoneMessage <- phoneMessage |> createRecord
                    setSuccessMessage "Phone Message created"
                    redirectTo PhoneMessagesAction

    action DeletePhoneMessageAction { phoneMessageId } = do
        phoneMessage <- fetch phoneMessageId
        deleteRecord phoneMessage
        setSuccessMessage "Phone Message deleted"
        redirectTo PhoneMessagesAction

buildPhoneMessage phoneMessage = phoneMessage
    |> fill @["toId","fromId", "sentAt", "body"]
    |> validateField #body nonEmpty
