module Application.Brain.Process (
    processIncomingMessage,
) where

import qualified Application.Action.ActionRunState as ActionRunState
import qualified Application.Action.ActionRunTime as ActionRunTime
import qualified Application.Action.SendMessageAction as SendMessageAction
import qualified Application.Base.PhoneNumber as PhoneNumber
import qualified Application.Brain.Act as Act
import qualified Application.Brain.Decide as Decide
import qualified Application.Brain.Observe as Observe
import qualified Application.Brain.Orient as Orient
import qualified Application.People.Person as Person
import qualified Application.Twilio.Query as Twilio.Query
import qualified Application.Twilio.View as Twilio.View
import Generated.Types
import IHP.ControllerPrelude

processIncomingMessage ::
    (?modelContext :: ModelContext) =>
    Text ->
    TwilioMessage ->
    IO ()
processIncomingMessage baseUrl twilioMessage = do
    workerPhoneNumberId <- get #id <$> fetchOne (get #fromId twilioMessage)
    botPhoneNumberId <- get #id <$> fetchOne (get #toId twilioMessage)
    workerId <- get #id <$> Person.fetchByPhoneNumber workerPhoneNumberId

    twilioMessageRows <- Twilio.Query.fetchById (get #id twilioMessage)
    let messages = Twilio.View.buildMessages twilioMessageRows

    case messages of
        message : _ -> do
            observations <- Observe.observe Observe.IncomingMessage {..}
            let situation = Orient.orient observations
            let plan = Decide.decide situation
            Act.act baseUrl plan
        [] -> pure ()
