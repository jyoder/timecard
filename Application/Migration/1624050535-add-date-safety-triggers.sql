drop trigger if exists "validate_timecard_entry_date" on "public"."timecard_entries";

drop function if exists "public"."trigger_validate_timecard_entry_date" ();

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

create trigger validate_timecard_entry_date_matches_timecard
    before insert or update on public.timecard_entries for each row
    execute procedure trigger_validate_timecard_entry_date_matches_timecard ();

create trigger validate_timecard_week_of_is_start_of_week
    before insert or update on public.timecards for each row
    execute procedure trigger_validate_timecard_week_of_is_start_of_week ();

create trigger validate_timecard_week_of_matches_timecard_entries
    before insert or update on public.timecards for each row
    execute procedure trigger_validate_timecard_week_of_matches_timecard_entries ();

