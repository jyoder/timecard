module Web.Controller.Persons where

import Web.Controller.Prelude
import Web.View.Persons.Edit
import Web.View.Persons.Index
import Web.View.Persons.New
import Web.View.Persons.Show

instance Controller PersonsController where
    action PersonsAction = do
        ensureIsUser
        persons <- query @Person |> fetch
        render IndexView {..}
    --
    action NewPersonAction = do
        ensureIsUser
        let person = newRecord
        render NewView {..}
    --
    action ShowPersonAction {personId} = do
        ensureIsUser
        person <- fetch personId
        render ShowView {..}
    --
    action EditPersonAction {personId} = do
        ensureIsUser
        person <- fetch personId
        render EditView {..}
    --
    action UpdatePersonAction {personId} = do
        ensureIsUser
        person <- fetch personId
        person
            |> buildPerson
            |> ifValid \case
                Left person -> render EditView {..}
                Right person -> do
                    person <- person |> updateRecord
                    setSuccessMessage "Person updated"
                    redirectTo EditPersonAction {..}
    --
    action CreatePersonAction = do
        ensureIsUser
        let person = newRecord @Person
        person
            |> buildPerson
            |> ifValid \case
                Left person -> render NewView {..}
                Right person -> do
                    person <- person |> createRecord
                    setSuccessMessage "Person created"
                    redirectTo PersonsAction
    --
    action DeletePersonAction {personId} = do
        ensureIsUser
        person <- fetch personId
        deleteRecord person
        setSuccessMessage "Person deleted"
        redirectTo PersonsAction

buildPerson :: (?context :: ControllerContext) => Person -> Person
buildPerson person =
    person
        |> fill @'["firstName", "lastName", "goesBy"]
        |> validateField #firstName nonEmpty
        |> validateField #lastName nonEmpty
        |> validateField #goesBy nonEmpty
