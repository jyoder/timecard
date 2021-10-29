create type "public"."audit_actions" as enum (
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
    'scheduled_message_deleted'
);

create table "public"."audit_entries" (
    "id" uuid not null default uuid_generate_v4 (),
    "created_at" timestamp with time zone not null default now(),
    "updated_at" timestamp with time zone not null default now(),
    "phone_number_id" uuid not null,
    "user_id" uuid,
    "action" audit_actions not null,
    "action_context" text not null
);

create index audit_entries_phone_number_id_index on public.audit_entries using btree (phone_number_id);

create unique index audit_entries_pkey on public.audit_entries using btree (id);

create index audit_entries_user_id_index on public.audit_entries using btree (user_id);

alter table "public"."audit_entries"
    add constraint "audit_entries_pkey" primary key using index "audit_entries_pkey";

alter table "public"."audit_entries"
    add constraint "audit_entries_ref_phone_number_id" foreign key (phone_number_id) references phone_numbers (id);

alter table "public"."audit_entries"
    add constraint "audit_entries_ref_user_id" foreign key (user_id) references users (id);

create trigger set_updated_at
    before update on public.audit_entries
    for each row
    execute function trigger_set_updated_at ();

