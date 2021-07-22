alter table "public"."timecard_entries"
    add column "clocked_in_at" time without time zone;

alter table "public"."timecard_entries"
    add column "clocked_out_at" time without time zone;

alter table "public"."timecard_entries"
    add column "lunch_duration" integer;

