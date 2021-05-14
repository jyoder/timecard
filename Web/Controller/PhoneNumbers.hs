module Web.Controller.PhoneNumbers where

import Web.Controller.Prelude
import Web.View.PhoneNumbers.Edit
import Web.View.PhoneNumbers.Index
import Web.View.PhoneNumbers.New
import Web.View.PhoneNumbers.Show

instance Controller PhoneNumbersController where
    action PhoneNumbersAction = do
        phoneNumbers <- query @PhoneNumber |> fetch
        render IndexView {..}
    --
    action NewPhoneNumberAction = do
        let phoneNumber = newRecord
        render NewView {..}
    --
    action ShowPhoneNumberAction {phoneNumberId} = do
        phoneNumber <- fetch phoneNumberId
        render ShowView {..}
    --
    action EditPhoneNumberAction {phoneNumberId} = do
        phoneNumber <- fetch phoneNumberId
        render EditView {..}
    --
    action UpdatePhoneNumberAction {phoneNumberId} = do
        phoneNumber <- fetch phoneNumberId
        phoneNumber
            |> buildPhoneNumber
            |> ifValid \case
                Left phoneNumber -> render EditView {..}
                Right phoneNumber -> do
                    phoneNumber <- phoneNumber |> updateRecord
                    setSuccessMessage "Phone Number updated"
                    redirectTo EditPhoneNumberAction {..}
    --
    action CreatePhoneNumberAction = do
        let phoneNumber = newRecord @PhoneNumber
        phoneNumber
            |> buildPhoneNumber
            |> ifValid \case
                Left phoneNumber -> render NewView {..}
                Right phoneNumber -> do
                    phoneNumber <- phoneNumber |> createRecord
                    setSuccessMessage "Phone Number created"
                    redirectTo PhoneNumbersAction
    --
    action DeletePhoneNumberAction {phoneNumberId} = do
        phoneNumber <- fetch phoneNumberId
        deleteRecord phoneNumber
        setSuccessMessage "Phone Number deleted"
        redirectTo PhoneNumbersAction

buildPhoneNumber phoneNumber =
    phoneNumber
        |> fill @'["number"]
        |> validateField #number isPhoneNumber
