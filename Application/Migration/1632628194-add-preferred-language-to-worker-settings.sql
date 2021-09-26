alter table "public"."worker_settings"
    add column "preferred_language" text not null default 'english'::text;

alter table "public"."worker_settings"
    add constraint "worker_settings_preferred_language_check" check (((preferred_language = 'english'::text) or (preferred_language = 'spanish'::text)));

