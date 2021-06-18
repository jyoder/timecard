alter table "public"."timecard_entries"
    alter column "timecard_id" set not null;

set check_function_bodies = off;

create or replace function public.trigger_validate_timecard_entry_date ()
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
        raise exception 'date does not match timecard week';
    end if;
end;
$function$;

create trigger validate_timecard_entry_date
    before update on public.timecard_entries
    for each row
    execute procedure trigger_validate_timecard_entry_date ();

