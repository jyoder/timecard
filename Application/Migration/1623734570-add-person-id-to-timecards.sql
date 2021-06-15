alter table "public"."action_run_states"
    drop constraint "valid_state";

alter table "public"."timecards"
    drop constraint "valid_state";

alter table "public"."timecards"
    add column "person_id" uuid not null;

create index timecards_person_id_index on public.timecards using btree (person_id);

create unique index timecards_person_id_week_of_key on public.timecards using btree (person_id, week_of);

alter table "public"."action_run_states"
    add constraint "action_run_states_state_check" check (((state = 'not_started'::text) or (state = 'started'::text) or (state = 'canceled'::text) or (state = 'finished'::text) or (state = 'failed'::text)));

alter table "public"."timecards"
    add constraint "timecards_person_id_week_of_key" unique using index "timecards_person_id_week_of_key";

alter table "public"."timecards"
    add constraint "timecards_ref_person_id" foreign key (person_id) references people (id);

alter table "public"."timecards"
    add constraint "timecards_state_check" check ((state = 'created'::text));

