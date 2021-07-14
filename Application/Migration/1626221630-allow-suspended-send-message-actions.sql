alter table "public"."action_run_states"
    drop constraint "action_run_states_state_check";

alter table "public"."process_events_jobs"
    alter column "status" drop default;

alter type "public"."job_status" rename to "job_status__old_version_to_be_dropped";

create type "public"."job_status" as enum (
    'job_status_not_started',
    'job_status_running',
    'job_status_failed',
    'job_status_timed_out',
    'job_status_succeeded',
    'job_status_retry'
);

alter table "public"."process_events_jobs"
    alter column status type "public"."job_status"
    using status::text "public"."job_status";

alter table "public"."process_events_jobs"
    alter column "status" set default 'job_status_not_started'::job_status;

drop type "public"."job_status__old_version_to_be_dropped";

alter table "public"."action_run_states"
    add constraint "action_run_states_state_check" check (((state = 'not_started'::text) or (state = 'suspended'::text) or (state = 'running'::text) or (state = 'canceled'::text) or (state = 'finished'::text) or (state = 'failed'::text)));

set check_function_bodies = off;

create or replace function public.trigger_validate_timecard_entry_date_matches_timecard ()
    returns trigger
    language plpgsql
    as $function$
begin
    if date_trunc('week', new.date) = (
        select
            timecards.week_of
        from
            timecards
        where
            timecards.id = new.timecard_id) then
        return NEW;
    else
        raise exception 'date must fall within the timecard week';
    end if;
end;
$function$;

create or replace function public.trigger_validate_timecard_week_of_is_start_of_week ()
    returns trigger
    language plpgsql
    as $function$
begin
    if new.week_of = date_trunc('week', new.week_of)::date then
        return NEW;
    else
        raise exception 'week_of must be the start of the week';
    end if;
end;
$function$;

create or replace function public.trigger_validate_timecard_week_of_matches_timecard_entries ()
    returns trigger
    language plpgsql
    as $function$
declare
    week_of date;
begin
    week_of := (
        select
            date_trunc('week', timecard_entries.date)::date week_of
        from
            timecard_entries
        where
            timecard_entries.timecard_id = new.id
        limit 1);
    if week_of is null or week_of = new.week_of then
        return NEW;
    else
        raise exception 'week_of must encompass timecard entry dates';
    end if;
end;
$function$;

