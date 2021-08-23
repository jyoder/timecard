-- pgFormatter-ignore

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.access_tokens DISABLE TRIGGER ALL;



ALTER TABLE public.access_tokens ENABLE TRIGGER ALL;


ALTER TABLE public.action_run_states DISABLE TRIGGER ALL;



ALTER TABLE public.action_run_states ENABLE TRIGGER ALL;


ALTER TABLE public.action_run_times DISABLE TRIGGER ALL;


ALTER TABLE public.action_run_times ENABLE TRIGGER ALL;


ALTER TABLE public.people DISABLE TRIGGER ALL;

INSERT INTO public.people (id, created_at, updated_at, first_name, last_name, goes_by) VALUES ('5419fec4-1380-475b-a6e7-87f1ea8870a6', '2021-05-11 22:58:57.614427-07', '2021-05-11 22:58:57.614427-07', 'Tim', 'Eckard', 'Tim the Bot');
INSERT INTO public.people (id, created_at, updated_at, first_name, last_name, goes_by) VALUES ('4383aa8e-d200-45cb-9c8e-0090f14457ef', '2021-05-11 22:07:30.127923-07', '2021-05-11 22:07:30.127923-07', 'Emma', 'Nazim', 'Emma');
INSERT INTO public.people (id, created_at, updated_at, first_name, last_name, goes_by) VALUES ('cfce1f7b-7617-4c5d-8349-e6640378e01e', '2021-05-12 23:36:10.535243-07', '2021-05-12 23:36:10.535243-07', 'Molly', 'Abbott', 'Molly');
INSERT INTO public.people (id, created_at, updated_at, first_name, last_name, goes_by) VALUES ('c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-14 14:09:54.579756-07', '2021-05-14 14:09:54.579756-07', 'John', 'Yoder', 'John');
INSERT INTO public.people (id, created_at, updated_at, first_name, last_name, goes_by) VALUES ('6e41e1a0-7f59-4195-ad39-546fcf1e6b79', '2021-05-14 14:10:03.581771-07', '2021-05-14 14:10:03.581771-07', 'Hillary', 'Yoder', 'Hillary');


ALTER TABLE public.people ENABLE TRIGGER ALL;


ALTER TABLE public.phone_numbers DISABLE TRIGGER ALL;

INSERT INTO public.phone_numbers (id, created_at, updated_at, number) VALUES ('a3067783-ada9-4f3d-988b-9ec0c89f959e', '2021-05-11 22:57:23.319527-07', '2021-05-11 22:57:23.319527-07', '+18058953296');
INSERT INTO public.phone_numbers (id, created_at, updated_at, number) VALUES ('6b261276-1ccf-4683-83a9-eda6a4e28b90', '2021-05-13 09:24:00.789685-07', '2021-05-13 09:24:00.789685-07', '+16616193290');
INSERT INTO public.phone_numbers (id, created_at, updated_at, number) VALUES ('c1e2457b-60ce-4e84-8ba0-a12020c49d40', '2021-05-11 22:23:59.97637-07', '2021-05-11 22:23:59.97637-07', '+12693593324');
INSERT INTO public.phone_numbers (id, created_at, updated_at, number) VALUES ('c1375981-0102-4132-8799-54f4fe3e0fbd', '2021-05-14 14:13:04.558717-07', '2021-05-14 14:13:04.558717-07', '+18054035926');
INSERT INTO public.phone_numbers (id, created_at, updated_at, number) VALUES ('aae83fea-7dd3-44eb-b51e-dbaf9a813db0', '2021-05-14 14:13:12.53256-07', '2021-05-14 14:13:12.53256-07', '+18054030600');


ALTER TABLE public.phone_numbers ENABLE TRIGGER ALL;


ALTER TABLE public.phone_contacts DISABLE TRIGGER ALL;

INSERT INTO public.phone_contacts (id, created_at, updated_at, person_id, phone_number_id) VALUES ('b2f62284-70f4-413e-9d14-d690a7877104', '2021-05-11 22:55:57.086991-07', '2021-05-11 22:55:57.086991-07', '4383aa8e-d200-45cb-9c8e-0090f14457ef', 'a3067783-ada9-4f3d-988b-9ec0c89f959e');
INSERT INTO public.phone_contacts (id, created_at, updated_at, person_id, phone_number_id) VALUES ('7de3ca81-a02e-4a7b-8b1b-e5388cc7b9d0', '2021-05-11 22:59:47.713103-07', '2021-05-11 22:59:47.713103-07', '5419fec4-1380-475b-a6e7-87f1ea8870a6', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40');
INSERT INTO public.phone_contacts (id, created_at, updated_at, person_id, phone_number_id) VALUES ('25389801-0db6-43f3-89e5-d767855e71c6', '2021-05-13 09:24:39.964652-07', '2021-05-13 09:24:39.964652-07', 'cfce1f7b-7617-4c5d-8349-e6640378e01e', '6b261276-1ccf-4683-83a9-eda6a4e28b90');
INSERT INTO public.phone_contacts (id, created_at, updated_at, person_id, phone_number_id) VALUES ('5c09699c-f953-4f2c-94ac-682a31063930', '2021-05-14 14:14:14.625618-07', '2021-05-14 14:14:14.625618-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', 'c1375981-0102-4132-8799-54f4fe3e0fbd');
INSERT INTO public.phone_contacts (id, created_at, updated_at, person_id, phone_number_id) VALUES ('f3c5ddd0-e305-4fe0-b208-8c913c18008c', '2021-05-14 14:14:37.524589-07', '2021-05-14 14:14:37.524589-07', '6e41e1a0-7f59-4195-ad39-546fcf1e6b79', 'aae83fea-7dd3-44eb-b51e-dbaf9a813db0');


ALTER TABLE public.phone_contacts ENABLE TRIGGER ALL;


ALTER TABLE public.process_events_jobs DISABLE TRIGGER ALL;

INSERT INTO public.process_events_jobs (id, created_at, updated_at, status, last_error, attempts_count, locked_at, locked_by) VALUES ('120e0bc3-81bb-453a-a1aa-226a5f8415ba', '2021-08-22 16:32:20.181043-07', '2021-08-22 20:39:11.608941-07', 'job_status_running', NULL, 1, '2021-08-22 16:32:20.188135-07', '3e5e623d-7f53-4bb7-b5ca-c92099002a8b');


ALTER TABLE public.process_events_jobs ENABLE TRIGGER ALL;


ALTER TABLE public.send_message_actions DISABLE TRIGGER ALL;



ALTER TABLE public.send_message_actions ENABLE TRIGGER ALL;


ALTER TABLE public.signings DISABLE TRIGGER ALL;



ALTER TABLE public.signings ENABLE TRIGGER ALL;


ALTER TABLE public.timecards DISABLE TRIGGER ALL;

INSERT INTO public.timecards (id, created_at, updated_at, week_of, person_id) VALUES ('d1bc51e7-a1f2-4503-9b15-1e8ee0fbea0d', '2021-08-18 21:59:22.819866-07', '2021-08-18 21:59:22.819866-07', '2021-08-16', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d');


ALTER TABLE public.timecards ENABLE TRIGGER ALL;


ALTER TABLE public.timecard_access_tokens DISABLE TRIGGER ALL;



ALTER TABLE public.timecard_access_tokens ENABLE TRIGGER ALL;


ALTER TABLE public.timecard_entries DISABLE TRIGGER ALL;

INSERT INTO public.timecard_entries (id, created_at, updated_at, date, job_name, hours_worked, work_done, invoice_translation, timecard_id, clocked_in_at, clocked_out_at, lunch_duration) VALUES ('4d0bbcf8-cfe5-4317-a4d5-4780944e6dee', '2021-08-18 21:59:22.819866-07', '2021-08-18 21:59:22.819866-07', '2021-08-18', 'John''s Parent''s House', 8, 'Test', 'Test', 'd1bc51e7-a1f2-4503-9b15-1e8ee0fbea0d', '07:00:00', '15:30:00', 30);


ALTER TABLE public.timecard_entries ENABLE TRIGGER ALL;


ALTER TABLE public.twilio_messages DISABLE TRIGGER ALL;

INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('8bc3933a-cc0f-446c-af4b-d2539e7ff813', '2021-08-18 21:58:28.077774-07', '2021-08-18 21:58:29.477766-07', '2010-04-01', 'SMf7f1c166d3b64888959e0a2bd167b786', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Test', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('04bfdc5d-94ed-4c03-acf1-f31957273311', '2021-08-22 14:35:30.205963-07', '2021-08-22 14:35:31.409934-07', '2010-04-01', 'SM91e98cecb20b412da2164e92e8e71dd2', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Hey John - I''ve got you at John''s Parent''s House today. Let me know what hours you worked and what you did when you have a chance. Thanks!', 0);


ALTER TABLE public.twilio_messages ENABLE TRIGGER ALL;


ALTER TABLE public.timecard_entry_messages DISABLE TRIGGER ALL;

INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('9c2bbacc-bc05-4769-a4b5-4617b41950ae', '2021-08-18 21:59:22.819866-07', '2021-08-18 21:59:22.819866-07', '4d0bbcf8-cfe5-4317-a4d5-4780944e6dee', '8bc3933a-cc0f-446c-af4b-d2539e7ff813');


ALTER TABLE public.timecard_entry_messages ENABLE TRIGGER ALL;


ALTER TABLE public.timecard_signings DISABLE TRIGGER ALL;



ALTER TABLE public.timecard_signings ENABLE TRIGGER ALL;


ALTER TABLE public.vertex_ai_entity_predictions DISABLE TRIGGER ALL;



ALTER TABLE public.vertex_ai_entity_predictions ENABLE TRIGGER ALL;


ALTER TABLE public.twilio_message_entities DISABLE TRIGGER ALL;



ALTER TABLE public.twilio_message_entities ENABLE TRIGGER ALL;


ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, created_at, updated_at) VALUES ('aa267118-f73f-45ab-9644-5705f21070e7', 'test@company.com', 'sha256|17|V6JGPzE2wOfao5X6hI+QPA==|5/99LLTnfGArcvfziM8eIvlOMN/syoKpKvvW3NaRBvM=', NULL, 0, '2021-06-10 22:27:20.261931-07', '2021-08-11 13:23:01.92718-07');


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.worker_settings DISABLE TRIGGER ALL;

INSERT INTO public.worker_settings (id, created_at, updated_at, person_id, send_daily_reminder_at, is_active) VALUES ('ade22074-a9a7-4ac1-80b9-a7b92eb40504', '2021-06-24 18:56:06.349431-07', '2021-06-24 18:56:06.349431-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '19:00:00', true);
INSERT INTO public.worker_settings (id, created_at, updated_at, person_id, send_daily_reminder_at, is_active) VALUES ('9652722d-5227-4c39-92f5-4adb32f6d5a6', '2021-06-24 18:57:37.004369-07', '2021-06-24 18:57:37.004369-07', '6e41e1a0-7f59-4195-ad39-546fcf1e6b79', '00:15:30', true);
INSERT INTO public.worker_settings (id, created_at, updated_at, person_id, send_daily_reminder_at, is_active) VALUES ('a4fad215-6ac9-437b-bc8d-b7a5b32275a6', '2021-06-24 18:57:59.718703-07', '2021-06-24 18:57:59.718703-07', 'cfce1f7b-7617-4c5d-8349-e6640378e01e', '00:15:30', true);
INSERT INTO public.worker_settings (id, created_at, updated_at, person_id, send_daily_reminder_at, is_active) VALUES ('23885399-9925-48dd-b922-41ff619eb0cd', '2021-06-24 18:58:16.658285-07', '2021-06-24 18:58:16.658285-07', '4383aa8e-d200-45cb-9c8e-0090f14457ef', '00:15:30', true);


ALTER TABLE public.worker_settings ENABLE TRIGGER ALL;


