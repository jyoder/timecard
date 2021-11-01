module Application.Audit.Entry where

import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

data MessageSentContext = MessageSentContext
    { twilioMessageId :: !(Id TwilioMessage)
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

data MessageProcessedContext = MessageProcessedContext
    { twilioMessageId :: !Text
    , messageBody :: !Text
    , situation :: !Text
    , plan :: !Text
    }
    deriving (Eq, Show)

data TimecardEntryContext = TimecardEntryContext
    { timecardEntryId :: !(Id TimecardEntry)
    , date :: !Day
    , jobName :: !Text
    , clockedInAt :: !(Maybe TimeOfDay)
    , clockedOutAt :: !(Maybe TimeOfDay)
    , lunchDuration :: !(Maybe Int)
    , hoursWorked :: !Double
    , workDone :: !Text
    , invoiceTranslation :: !Text
    }
    deriving (Eq, Show)

data ScheduledMessageContext = ScheduledMessageContext
    { sendMessageActionId :: !(Id SendMessageAction)
    , sendAt :: !UTCTime
    , body :: !Text
    }
    deriving (Eq, Show)

createMessageSent ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    TwilioMessage ->
    Text ->
    IO AuditEntry
createMessageSent userId twilioMessage fromPhoneNumber =
    createEntry
        userId
        (get #toId twilioMessage)
        MessageSent
        (show $ messageSentContext twilioMessage fromPhoneNumber)

messageSentContext :: TwilioMessage -> Text -> MessageSentContext
messageSentContext twilioMessage fromPhoneNumber =
    MessageSentContext
        { twilioMessageId = get #id twilioMessage
        , twilioMessageSid = get #messageSid twilioMessage
        , fromPhoneNumber = fromPhoneNumber
        , messageBody = get #body twilioMessage
        }

createMessageReceived ::
    (?modelContext :: ModelContext) =>
    TwilioMessage ->
    Text ->
    IO AuditEntry
createMessageReceived twilioMessage toPhoneNumber =
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

createMessageProcessed ::
    (?modelContext :: ModelContext) =>
    TwilioMessage ->
    Text ->
    Text ->
    IO AuditEntry
createMessageProcessed twilioMessage situation plan =
    createEntry
        Nothing
        (get #fromId twilioMessage)
        MessageProcessed
        (show $ messageProcessedContext twilioMessage situation plan)

messageProcessedContext :: TwilioMessage -> Text -> Text -> MessageProcessedContext
messageProcessedContext twilioMessage situation plan =
    MessageProcessedContext
        { twilioMessageId = show $ get #id twilioMessage
        , messageBody = get #body twilioMessage
        , ..
        }

createTimecardEntryCreated ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    Id PhoneNumber ->
    TimecardEntry ->
    IO AuditEntry
createTimecardEntryCreated userId phoneNumberId timecardEntry =
    createEntry
        userId
        phoneNumberId
        TimecardEntryCreated
        ( show $
            TimecardEntryContext
                { timecardEntryId = get #id timecardEntry
                , date = get #date timecardEntry
                , jobName = get #jobName timecardEntry
                , clockedInAt = get #clockedInAt timecardEntry
                , clockedOutAt = get #clockedOutAt timecardEntry
                , lunchDuration = get #lunchDuration timecardEntry
                , hoursWorked = get #hoursWorked timecardEntry
                , workDone = get #workDone timecardEntry
                , invoiceTranslation = get #invoiceTranslation timecardEntry
                }
        )

createTimecardEntryEdited ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    Id PhoneNumber ->
    TimecardEntry ->
    IO AuditEntry
createTimecardEntryEdited userId phoneNumberId timecardEntry =
    createEntry
        userId
        phoneNumberId
        TimecardEntryEdited
        ( show $
            TimecardEntryContext
                { timecardEntryId = get #id timecardEntry
                , date = get #date timecardEntry
                , jobName = get #jobName timecardEntry
                , clockedInAt = get #clockedInAt timecardEntry
                , clockedOutAt = get #clockedOutAt timecardEntry
                , lunchDuration = get #lunchDuration timecardEntry
                , hoursWorked = get #hoursWorked timecardEntry
                , workDone = get #workDone timecardEntry
                , invoiceTranslation = get #invoiceTranslation timecardEntry
                }
        )

createTimecardEntryDeleted ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    Id PhoneNumber ->
    TimecardEntry ->
    IO AuditEntry
createTimecardEntryDeleted userId phoneNumberId timecardEntry =
    createEntry
        userId
        phoneNumberId
        TimecardEntryDeleted
        ( show $
            TimecardEntryContext
                { timecardEntryId = get #id timecardEntry
                , date = get #date timecardEntry
                , jobName = get #jobName timecardEntry
                , clockedInAt = get #clockedInAt timecardEntry
                , clockedOutAt = get #clockedOutAt timecardEntry
                , lunchDuration = get #lunchDuration timecardEntry
                , hoursWorked = get #hoursWorked timecardEntry
                , workDone = get #workDone timecardEntry
                , invoiceTranslation = get #invoiceTranslation timecardEntry
                }
        )

createReviewLinkGenerated ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    Id PhoneNumber ->
    Text ->
    IO AuditEntry
createReviewLinkGenerated userId phoneNumberId =
    createEntry
        userId
        phoneNumberId
        ReviewLinkGenerated

createReviewSigned ::
    (?modelContext :: ModelContext) =>
    Id PhoneNumber ->
    Text ->
    IO AuditEntry
createReviewSigned phoneNumberId =
    createEntry
        Nothing
        phoneNumberId
        ReviewSigned

createDailyReminderScheduled ::
    (?modelContext :: ModelContext) =>
    SendMessageAction ->
    UTCTime ->
    IO AuditEntry
createDailyReminderScheduled sendMessageAction sendAt =
    createEntry
        Nothing
        (get #toId sendMessageAction)
        DailyReminderScheduled
        ( show
            ScheduledMessageContext
                { sendMessageActionId = get #id sendMessageAction
                , body = get #body sendMessageAction
                , ..
                }
        )

createReviewRequestScheduled ::
    (?modelContext :: ModelContext) =>
    SendMessageAction ->
    UTCTime ->
    IO AuditEntry
createReviewRequestScheduled sendMessageAction sendAt =
    createEntry
        Nothing
        (get #toId sendMessageAction)
        ReviewRequestScheduled
        ( show
            ScheduledMessageContext
                { sendMessageActionId = get #id sendMessageAction
                , body = get #body sendMessageAction
                , ..
                }
        )

createScheduledMessageEdited ::
    (?modelContext :: ModelContext) =>
    Id User ->
    SendMessageAction ->
    UTCTime ->
    IO AuditEntry
createScheduledMessageEdited userId sendMessageAction sendAt =
    createEntry
        (Just userId)
        (get #toId sendMessageAction)
        ScheduledMessageEdited
        ( show
            ScheduledMessageContext
                { sendMessageActionId = get #id sendMessageAction
                , body = get #body sendMessageAction
                , ..
                }
        )

createScheduledMessageSuspended ::
    (?modelContext :: ModelContext) =>
    SendMessageAction ->
    UTCTime ->
    IO AuditEntry
createScheduledMessageSuspended sendMessageAction sendAt =
    createEntry
        Nothing
        (get #toId sendMessageAction)
        ScheduledMessageSuspended
        ( show
            ScheduledMessageContext
                { sendMessageActionId = get #id sendMessageAction
                , body = get #body sendMessageAction
                , ..
                }
        )

createScheduledMessageResumed ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    SendMessageAction ->
    UTCTime ->
    IO AuditEntry
createScheduledMessageResumed userId sendMessageAction sendAt =
    createEntry
        userId
        (get #toId sendMessageAction)
        ScheduledMessageResumed
        ( show
            ScheduledMessageContext
                { sendMessageActionId = get #id sendMessageAction
                , body = get #body sendMessageAction
                , ..
                }
        )

createScheduledMessageCanceled ::
    (?modelContext :: ModelContext) =>
    Maybe (Id User) ->
    SendMessageAction ->
    UTCTime ->
    IO AuditEntry
createScheduledMessageCanceled userId sendMessageAction sendAt =
    createEntry
        userId
        (get #toId sendMessageAction)
        ScheduledMessageCanceled
        ( show
            ScheduledMessageContext
                { sendMessageActionId = get #id sendMessageAction
                , body = get #body sendMessageAction
                , ..
                }
        )

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
