

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

INSERT INTO public.action_run_states (id, created_at, updated_at, state) VALUES ('b2f58b89-e04e-4508-a8fa-7749f9c1416e', '2021-06-24 23:34:35.316513-07', '2021-06-24 23:35:10.264679-07', 'canceled');


ALTER TABLE public.action_run_states ENABLE TRIGGER ALL;


ALTER TABLE public.action_run_times DISABLE TRIGGER ALL;

INSERT INTO public.action_run_times (id, created_at, updated_at, runs_at, action_run_state_id) VALUES ('1e586619-7ce7-4cd2-8659-8d3800769cb5', '2021-06-24 23:34:35.316513-07', '2021-06-24 23:34:35.316513-07', '2021-06-25 19:00:00-07', 'b2f58b89-e04e-4508-a8fa-7749f9c1416e');


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



ALTER TABLE public.process_events_jobs ENABLE TRIGGER ALL;


ALTER TABLE public.send_message_actions DISABLE TRIGGER ALL;

INSERT INTO public.send_message_actions (id, created_at, updated_at, body, from_id, to_id, action_run_state_id) VALUES ('633a0a1c-d448-49d2-bc2c-6e740faf1164', '2021-06-24 23:34:35.316513-07', '2021-06-24 23:34:35.316513-07', 'Hey John - I''ve got you at Wendy''s today. Let me know what hours you worked and what you did when you have a chance. Thanks!', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'b2f58b89-e04e-4508-a8fa-7749f9c1416e');


ALTER TABLE public.send_message_actions ENABLE TRIGGER ALL;


ALTER TABLE public.signings DISABLE TRIGGER ALL;



ALTER TABLE public.signings ENABLE TRIGGER ALL;


ALTER TABLE public.timecards DISABLE TRIGGER ALL;

INSERT INTO public.timecards (id, created_at, updated_at, week_of, person_id) VALUES ('844bc6aa-14ba-4971-bfc4-7be08877813d', '2021-06-24 23:34:35.307617-07', '2021-06-24 23:34:35.307617-07', '2021-06-21', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d');


ALTER TABLE public.timecards ENABLE TRIGGER ALL;


ALTER TABLE public.timecard_access_tokens DISABLE TRIGGER ALL;



ALTER TABLE public.timecard_access_tokens ENABLE TRIGGER ALL;


ALTER TABLE public.timecard_entries DISABLE TRIGGER ALL;

INSERT INTO public.timecard_entries (id, created_at, updated_at, date, job_name, hours_worked, work_done, invoice_translation, timecard_id) VALUES ('85206169-9ea5-42ca-b24c-13682cc23711', '2021-06-24 23:34:35.307617-07', '2021-06-24 23:34:35.307617-07', '2021-06-24', 'Wendy''s', 8, 'Sure, I worked at Wendy''s for 8 hours.', 'John Y.  I worked at Wendy''s for 8 hours.', '844bc6aa-14ba-4971-bfc4-7be08877813d');


ALTER TABLE public.timecard_entries ENABLE TRIGGER ALL;


ALTER TABLE public.twilio_messages DISABLE TRIGGER ALL;

INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('17633d62-ceeb-4bfe-9af8-b0194f31d84d', '2021-06-24 23:33:03.194874-07', '2021-06-24 23:33:04.547602-07', '2010-04-01', 'SM6be62c1a0dad4cef998a5c2cb3930dde', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Hi John, can you send over your time info for the day?', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('b23406eb-366e-4a5b-8cb6-84756fee5464', '2021-06-24 23:33:47.999719-07', '2021-06-24 23:33:47.999719-07', '2010-04-01', 'SM0052f88d972a7f6480bd9e8e6f28cf4e', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'received', 'Sure, I worked at Wendy''s for 8 hours.', 0);


ALTER TABLE public.twilio_messages ENABLE TRIGGER ALL;


ALTER TABLE public.timecard_entry_messages DISABLE TRIGGER ALL;

INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('8057f5a5-28c5-40ad-ab66-d4bcaef215e7', '2021-06-24 23:34:35.307617-07', '2021-06-24 23:34:35.307617-07', '85206169-9ea5-42ca-b24c-13682cc23711', 'b23406eb-366e-4a5b-8cb6-84756fee5464');


ALTER TABLE public.timecard_entry_messages ENABLE TRIGGER ALL;


ALTER TABLE public.timecard_signings DISABLE TRIGGER ALL;



ALTER TABLE public.timecard_signings ENABLE TRIGGER ALL;


ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts, created_at, updated_at) VALUES ('aa267118-f73f-45ab-9644-5705f21070e7', 'test@company.com', 'sha256|17|V6JGPzE2wOfao5X6hI+QPA==|5/99LLTnfGArcvfziM8eIvlOMN/syoKpKvvW3NaRBvM=', NULL, 0, '2021-06-10 22:27:20.261931-07', '2021-06-11 10:53:14.096509-07');


ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.worker_preferences DISABLE TRIGGER ALL;

INSERT INTO public.worker_preferences (id, created_at, updated_at, person_id, send_daily_reminder_at) VALUES ('ade22074-a9a7-4ac1-80b9-a7b92eb40504', '2021-06-24 18:56:06.349431-07', '2021-06-24 18:56:06.349431-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '19:00:00');
INSERT INTO public.worker_preferences (id, created_at, updated_at, person_id, send_daily_reminder_at) VALUES ('9652722d-5227-4c39-92f5-4adb32f6d5a6', '2021-06-24 18:57:37.004369-07', '2021-06-24 18:57:37.004369-07', '6e41e1a0-7f59-4195-ad39-546fcf1e6b79', '00:15:30');
INSERT INTO public.worker_preferences (id, created_at, updated_at, person_id, send_daily_reminder_at) VALUES ('a4fad215-6ac9-437b-bc8d-b7a5b32275a6', '2021-06-24 18:57:59.718703-07', '2021-06-24 18:57:59.718703-07', 'cfce1f7b-7617-4c5d-8349-e6640378e01e', '00:15:30');
INSERT INTO public.worker_preferences (id, created_at, updated_at, person_id, send_daily_reminder_at) VALUES ('23885399-9925-48dd-b922-41ff619eb0cd', '2021-06-24 18:58:16.658285-07', '2021-06-24 18:58:16.658285-07', '4383aa8e-d200-45cb-9c8e-0090f14457ef', '00:15:30');


ALTER TABLE public.worker_preferences ENABLE TRIGGER ALL;


