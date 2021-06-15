create table "public"."timecards" (
    "id" uuid not null default uuid_generate_v4 (),
    "created_at" timestamp with time zone not null default now(),
    "updated_at" timestamp with time zone not null default now(),
    "week_of" date not null,
    "state" text not null default 'created' ::text
);

alter table "public"."timecard_entries"
    add column "timecard_id" uuid;

create index timecard_entries_timecard_id_index on public.timecard_entries using btree (timecard_id);

create unique index timecards_pkey on public.timecards using btree (id);

alter table "public"."timecards"
    add constraint "timecards_pkey" primary key using index "timecards_pkey";

alter table "public"."action_run_states"
    add constraint "valid_state" check (((state = 'not_started'::text) or (state = 'started'::text) or (state = 'canceled'::text) or (state = 'finished'::text) or (state = 'failed'::text)));

alter table "public"."timecard_entries"
    add constraint "timecard_entries_ref_timecard_id" foreign key (timecard_id) references timecards (id);

alter table "public"."timecards"
    add constraint "valid_state" check ((state = 'created'::text));

create trigger set_updated_at
    before update on public.timecards for each row
    execute procedure trigger_set_updated_at ();

