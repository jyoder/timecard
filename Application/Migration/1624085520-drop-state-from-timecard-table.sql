alter table "public"."timecards"
    drop constraint "timecards_state_check";

alter table "public"."timecards"
    drop column "state";

