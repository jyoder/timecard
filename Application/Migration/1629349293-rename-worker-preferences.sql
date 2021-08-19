drop trigger if exists "set_updated_at" on "public"."worker_preferences";

alter table "public"."worker_preferences"
    drop constraint "worker_preferences_ref_person_id";

alter table "public"."worker_preferences"
    drop constraint "worker_preferences_pkey";

drop index if exists "public"."worker_preferences_person_id_index";

drop index if exists "public"."worker_preferences_pkey";

alter table "public"."worker_preferences" rename to "public"."worker_settings";

create index worker_settings_person_id_index on public.worker_settings using btree (person_id);

create unique index worker_settings_pkey on public.worker_settings using btree (id);

alter table "public"."worker_settings"
    add constraint "worker_settings_pkey" primary key using index "worker_settings_pkey";

alter table "public"."worker_settings"
    add constraint "worker_settings_ref_person_id" foreign key (person_id) references people (id);

create trigger set_updated_at
    before update on public.worker_settings for each row
    execute procedure trigger_set_updated_at ();

