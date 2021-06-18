alter table "public"."timecard_entries"
    drop constraint "timecard_entries_ref_person_id";

drop index if exists "public"."timecard_entries_person_id_index";

alter table "public"."timecard_entries"
    drop column "person_id";

