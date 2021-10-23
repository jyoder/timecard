alter table "public"."access_tokens"
    add constraint "access_tokens_value_check" check ((btrim(value) <> ''::text));

alter table "public"."people"
    add constraint "people_first_name_check" check ((btrim(first_name) <> ''::text));

alter table "public"."people"
    add constraint "people_goes_by_check" check ((btrim(goes_by) <> ''::text));

alter table "public"."people"
    add constraint "people_last_name_check" check ((btrim(last_name) <> ''::text));

alter table "public"."send_message_actions"
    add constraint "send_message_actions_body_check" check ((btrim(body) <> ''::text));

alter table "public"."signings"
    add constraint "signings_ip_address_check" check ((btrim(ip_address) <> ''::text));

alter table "public"."signings"
    add constraint "signings_name_check" check ((btrim(name) <> ''::text));

alter table "public"."timecard_entries"
    add constraint "timecard_entries_invoice_translation_check" check ((btrim(invoice_translation) <> ''::text));

alter table "public"."timecard_entries"
    add constraint "timecard_entries_job_name_check" check ((btrim(job_name) <> ''::text));

alter table "public"."timecard_entries"
    add constraint "timecard_entries_work_done_check" check ((btrim(work_done) <> ''::text));

alter table "public"."users"
    add constraint "users_email_check" check ((btrim(email) <> ''::text));

alter table "public"."users"
    add constraint "users_password_hash_check" check ((btrim(password_hash) <> ''::text));

create trigger set_updated_at
    before update on public.fetch_entity_prediction_jobs for each row
    execute function trigger_set_updated_at ();

