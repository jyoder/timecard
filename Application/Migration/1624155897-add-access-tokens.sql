create table "public"."access_tokens" (
    "id" uuid not null default uuid_generate_v4 (),
    "created_at" timestamp with time zone not null default now(),
    "updated_at" timestamp with time zone not null default now(),
    "value" text not null,
    "expires_at" timestamp with time zone not null,
    "is_revoked" boolean not null default false
);

create table "public"."timecard_access_tokens" (
    "id" uuid not null default uuid_generate_v4 (),
    "created_at" timestamp with time zone not null default now(),
    "updated_at" timestamp with time zone not null default now(),
    "timecard_id" uuid not null,
    "access_token_id" uuid not null
);

create unique index access_tokens_pkey on public.access_tokens using btree (id);

create index timecard_access_tokens_access_token_id_index on public.timecard_access_tokens using btree (access_token_id);

create unique index timecard_access_tokens_access_token_id_key on public.timecard_access_tokens using btree (access_token_id);

create unique index timecard_access_tokens_pkey on public.timecard_access_tokens using btree (id);

create index timecard_access_tokens_timecard_id_index on public.timecard_access_tokens using btree (timecard_id);

alter table "public"."access_tokens"
    add constraint "access_tokens_pkey" primary key using index "access_tokens_pkey";

alter table "public"."timecard_access_tokens"
    add constraint "timecard_access_tokens_pkey" primary key using index "timecard_access_tokens_pkey";

alter table "public"."timecard_access_tokens"
    add constraint "timecard_access_tokens_access_token_id_key" unique using index "timecard_access_tokens_access_token_id_key";

alter table "public"."timecard_access_tokens"
    add constraint "timecard_access_tokens_ref_access_token_id" foreign key (access_token_id) references access_tokens (id);

alter table "public"."timecard_access_tokens"
    add constraint "timecard_access_tokens_ref_timecard_id" foreign key (timecard_id) references timecards (id);

create trigger set_updated_at
    before update on public.access_tokens for each row
    execute procedure trigger_set_updated_at ();

create trigger set_updated_at
    before update on public.timecard_access_tokens for each row
    execute procedure trigger_set_updated_at ();

