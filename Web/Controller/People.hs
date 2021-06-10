module Web.Controller.People where

import qualified Application.Base.People as People
import Web.Controller.Prelude
import Web.View.People.Edit
import Web.View.People.Index
import Web.View.People.New
import Web.View.People.Show

instance Controller PeopleController where
    action PeopleAction = do
        ensureIsUser
        people <- query @Person |> fetch
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
                    redirectTo PeopleAction
    --
    action DeletePersonAction {personId} = do
        ensureIsUser
        person <- fetch personId
        deleteRecord person
        setSuccessMessage "Person deleted"
        redirectTo PeopleAction

buildPerson :: (?context :: ControllerContext) => Person -> Person
buildPerson person =
    person
        |> fill @'["firstName", "lastName", "goesBy"]
        |> People.validate
