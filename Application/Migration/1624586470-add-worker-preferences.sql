create table "public"."worker_preferences" (
    "id" uuid not null default uuid_generate_v4 (),
    "created_at" timestamp with time zone not null default now(),
    "updated_at" timestamp with time zone not null default now(),
    "person_id" uuid not null,
    "send_daily_reminder_at" time without time zone not null
);

create index worker_preferences_person_id_index on public.worker_preferences using btree (person_id);

create unique index worker_preferences_pkey on public.worker_preferences using btree (id);

alter table "public"."worker_preferences"
    add constraint "worker_preferences_pkey" primary key using index "worker_preferences_pkey";

alter table "public"."worker_preferences"
    add constraint "worker_preferences_ref_person_id" foreign key (person_id) references people (id);

create trigger set_updated_at
    before update on public.signings for each row
    execute procedure trigger_set_updated_at ();

create trigger set_updated_at
    before update on public.timecard_signings for each row
    execute procedure trigger_set_updated_at ();

create trigger set_updated_at
    before update on public.worker_preferences for each row
    execute procedure trigger_set_updated_at ();

