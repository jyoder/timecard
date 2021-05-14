module Web.Controller.PhoneContacts where

import Web.Controller.Prelude
import Web.View.PhoneContacts.Edit
import Web.View.PhoneContacts.Index
import Web.View.PhoneContacts.New
import Web.View.PhoneContacts.Show

instance Controller PhoneContactsController where
    action PhoneContactsAction = do
        phoneContacts <- query @PhoneContact |> fetch
        render IndexView {..}
    --
    action NewPhoneContactAction = do
        let phoneContact = newRecord
        render NewView {..}
    --
    action ShowPhoneContactAction {phoneContactId} = do
        phoneContact <- fetch phoneContactId
        render ShowView {..}
    --
    action EditPhoneContactAction {phoneContactId} = do
        phoneContact <- fetch phoneContactId
        render EditView {..}
    --
    action UpdatePhoneContactAction {phoneContactId} = do
        phoneContact <- fetch phoneContactId
        phoneContact
            |> buildPhoneContact
            |> ifValid \case
                Left phoneContact -> render EditView {..}
                Right phoneContact -> do
                    phoneContact <- phoneContact |> updateRecord
                    setSuccessMessage "Phone Contact updated"
                    redirectTo EditPhoneContactAction {..}
    --
    action CreatePhoneContactAction = do
        let phoneContact = newRecord @PhoneContact
        phoneContact
            |> buildPhoneContact
            |> ifValid \case
                Left phoneContact -> render NewView {..}
                Right phoneContact -> do
                    phoneContact <- phoneContact |> createRecord
                    setSuccessMessage "Phone Contact created"
                    redirectTo PhoneContactsAction
    --
    action DeletePhoneContactAction {phoneContactId} = do
        phoneContact <- fetch phoneContactId
        deleteRecord phoneContact
        setSuccessMessage "Phone Contact deleted"
        redirectTo PhoneContactsAction

buildPhoneContact phoneContact =
    phoneContact
        |> fill @["personId", "phoneNumberId"]
