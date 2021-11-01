module Web.Controller.AuditEntries where

import qualified Application.Audit.Query as Audit.Query
import qualified Application.Base.PhoneNumber as PhoneNumber
import qualified Application.People.Query as People.Query
import qualified Application.People.View as People.View
import Web.Controller.Prelude
import Web.View.AuditEntries.Index

instance Controller AuditEntriesController where
    beforeAction = ensureIsUser

    action AuditEntriesAction = do
        let currentColumn = PeopleColumn

        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers

        personSelection <- case people of
            firstPerson : _ -> do
                let selectedPersonId = get #id firstPerson
                selectedPerson <- fetch selectedPersonId
                phoneNumber <- PhoneNumber.fetchByPerson $ get #id selectedPerson
                auditEntries <- Audit.Query.fetchByPhoneNumber $ get #id phoneNumber

                let personSelection = PersonSelected {..}
                pure PersonSelected {..}
            [] -> pure NoPersonSelected

        render IndexView {..}
    --
    action AuditEntriesPersonSelectionAction {..} = do
        let currentColumn = maybe PeopleColumn paramToColumn column

        people <-
            People.View.buildPeople
                <$> People.Query.fetchActiveWorkers
        selectedPerson <- fetch selectedPersonId
        phoneNumber <- PhoneNumber.fetchByPerson $ get #id selectedPerson
        auditEntries <- Audit.Query.fetchByPhoneNumber $ get #id phoneNumber

        let personSelection = PersonSelected {..}

        render IndexView {..}

paramToColumn :: Text -> Column
paramToColumn "people" = PeopleColumn
paramToColumn "entries" = EntriesColumn
paramToColumn _ = PeopleColumn
