create table "public"."signings" (
    "id" uuid not null default uuid_generate_v4 (),
    "created_at" timestamp with time zone not null default now(),
    "updated_at" timestamp with time zone not null default now(),
    "signed_at" timestamp with time zone not null default now(),
    "name" text not null,
    "ip_address" text not null
);

create table "public"."timecard_signings" (
    "id" uuid not null default uuid_generate_v4 (),
    "created_at" timestamp with time zone not null default now(),
    "updated_at" timestamp with time zone not null default now(),
    "timecard_id" uuid not null,
    "signing_id" uuid not null
);

create unique index signings_pkey on public.signings using btree (id);

create unique index timecard_signings_pkey on public.timecard_signings using btree (id);

create index timecard_signings_signing_id_index on public.timecard_signings using btree (signing_id);

create index timecard_signings_timecard_id_index on public.timecard_signings using btree (timecard_id);

create unique index timecard_signings_timecard_id_signing_id_key on public.timecard_signings using btree (timecard_id, signing_id);

alter table "public"."signings"
    add constraint "signings_pkey" primary key using index "signings_pkey";

alter table "public"."timecard_signings"
    add constraint "timecard_signings_pkey" primary key using index "timecard_signings_pkey";

alter table "public"."timecard_signings"
    add constraint "timecard_signings_ref_signing_id" foreign key (signing_id) references signings (id);

alter table "public"."timecard_signings"
    add constraint "timecard_signings_ref_timecard_id" foreign key (timecard_id) references timecards (id);

alter table "public"."timecard_signings"
    add constraint "timecard_signings_timecard_id_signing_id_key" unique using index "timecard_signings_timecard_id_signing_id_key";

