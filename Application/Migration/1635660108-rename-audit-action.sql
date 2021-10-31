alter type audit_actions rename to audit_actions__old_version_to_be_dropped;

create type audit_actions as enum (
    'message_sent',
    'message_received',
    'message_processed',
    'timecard_entry_created',
    'timecard_entry_edited',
    'timecard_entry_deleted',
    'review_link_generated',
    'review_signed',
    'daily_reminder_scheduled',
    'review_request_scheduled',
    'scheduled_message_edited',
    'scheduled_message_suspended',
    'scheduled_message_resumed',
    'scheduled_message_canceled'
);

alter table audit_entries
    alter column action type audit_actions
    using action::text audit_actions;

drop type audit_actions__old_version_to_be_dropped;

