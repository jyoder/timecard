module Web.Controller.PhoneContacts where

import Web.Controller.Prelude
import Web.View.PhoneContacts.Edit
import Web.View.PhoneContacts.Index
import Web.View.PhoneContacts.New
import Web.View.PhoneContacts.Show

instance Controller PhoneContactsController where
    action PhoneContactsAction = do
        ensureIsUser
        phoneContacts <- query @PhoneContact |> fetch
        render IndexView {..}
    --
    action NewPhoneContactAction = do
        ensureIsUser
        let phoneContact = newRecord
        render NewView {..}
    --
    action ShowPhoneContactAction {phoneContactId} = do
        ensureIsUser
        phoneContact <- fetch phoneContactId
        render ShowView {..}
    --
    action EditPhoneContactAction {phoneContactId} = do
        ensureIsUser
        phoneContact <- fetch phoneContactId
        render EditView {..}
    --
    action UpdatePhoneContactAction {phoneContactId} = do
        ensureIsUser
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
        ensureIsUser
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
        ensureIsUser
        phoneContact <- fetch phoneContactId
        deleteRecord phoneContact
        setSuccessMessage "Phone Contact deleted"
        redirectTo PhoneContactsAction

buildPhoneContact :: (?context :: ControllerContext) => PhoneContact -> PhoneContact
buildPhoneContact phoneContact =
    phoneContact
        |> fill @["personId", "phoneNumberId"]
