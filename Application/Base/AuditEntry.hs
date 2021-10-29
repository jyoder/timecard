module Application.Base.AuditEntry where

import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

data MessageSentContext = MessageSentContext
    { twilioMessageId :: !Text
    , twilioMessageSid :: !Text
    , fromPhoneNumber :: !Text
    , messageBody :: !Text
    }
    deriving (Eq, Show)

data MessageReceivedContext = MessageReceivedContext
    { twilioMessageId :: !Text
    , twilioMessageSid :: !Text
    , toPhoneNumber :: !Text
    , messageBody :: !Text
    }
    deriving (Eq, Show)

createMessageSentEntry ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    TwilioMessage ->
    Text ->
    IO AuditEntry
createMessageSentEntry userId twilioMessage fromPhoneNumber =
    createEntry
        userId
        (get #toId twilioMessage)
        MessageSent
        (show $ messageSentContext twilioMessage fromPhoneNumber)

messageSentContext :: TwilioMessage -> Text -> MessageSentContext
messageSentContext twilioMessage fromPhoneNumber =
    MessageSentContext
        { twilioMessageId = show $ get #id twilioMessage
        , twilioMessageSid = get #messageSid twilioMessage
        , fromPhoneNumber = fromPhoneNumber
        , messageBody = get #body twilioMessage
        }

createMessageReceivedEntry ::
    (?modelContext :: ModelContext) =>
    TwilioMessage ->
    Text ->
    IO AuditEntry
createMessageReceivedEntry twilioMessage toPhoneNumber =
    createEntry
        Nothing
        (get #fromId twilioMessage)
        MessageReceived
        (show $ messageReceivedContext twilioMessage toPhoneNumber)

messageReceivedContext :: TwilioMessage -> Text -> MessageReceivedContext
messageReceivedContext twilioMessage toPhoneNumber =
    MessageReceivedContext
        { twilioMessageId = show $ get #id twilioMessage
        , twilioMessageSid = get #messageSid twilioMessage
        , toPhoneNumber = toPhoneNumber
        , messageBody = get #body twilioMessage
        }

createEntry ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    Id PhoneNumber ->
    AuditAction ->
    Text ->
    IO AuditEntry
createEntry userId phoneNumberId auditAction actionContext =
    newRecord @AuditEntry
        |> set #userId userId
        |> set #phoneNumberId phoneNumberId
        |> set #action auditAction
        |> set #actionContext actionContext
        |> createRecord
