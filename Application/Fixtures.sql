

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

INSERT INTO public.process_events_jobs (id, created_at, updated_at, status, last_error, attempts_count, locked_at, locked_by) VALUES ('a39d6e53-55ab-4d67-a744-7a81a9d0fddc', '2021-06-06 13:57:01.824153-07', '2021-06-06 13:57:01.849111-07', 'job_status_running', 'Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at ./Web/Job/ProcessEvents.hs:80:16 in main:Web.Job.ProcessEvents', 3, '2021-06-06 13:57:01.853923-07', 'a8141f09-3139-48cc-ae2b-ca90721a695c');


ALTER TABLE public.process_events_jobs ENABLE TRIGGER ALL;


ALTER TABLE public.send_message_actions DISABLE TRIGGER ALL;



ALTER TABLE public.send_message_actions ENABLE TRIGGER ALL;


ALTER TABLE public.timecard_entries DISABLE TRIGGER ALL;

INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('dc95f428-1527-4f62-9fa8-9bf8b3a8466d', '2021-05-19 23:59:12.856885-07', '2021-05-19 23:59:12.856885-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-18 18:44:40.959-07', 'Burger King', 8, 'Boogers
Hamburgers
Pickles

', 'Boogers
Hamburgers
Pickles

');
INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('9f12ff91-6eb2-421e-b6bd-c7dd4310fd5f', '2021-05-19 23:59:33.272609-07', '2021-05-19 23:59:33.272609-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-17 22:37:05.928-07', 'McDonalds', 8, 'Sent from your Twilio trial account - hello

', 'Sent from your Twilio trial account - hello

');
INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('99b923ae-d69a-4238-9a8d-7ab75410cf18', '2021-05-20 15:28:36.51917-07', '2021-05-20 15:28:36.51917-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-19 16:15:00-07', 'Wendy''s New', 8, 'Sent from your Twilio trial account - </textarea> <p>Another hello</p>

', 'Sent from your Twilio trial account - </textarea> <p>Another hello</p>

');
INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('86b8ac98-939f-4d04-b904-d79a49ed9c31', '2021-05-20 15:29:07.044201-07', '2021-05-20 15:29:07.044201-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-19 16:15:00-07', 'Wendy''s New again', 8, 'Sent from your Twilio trial account - </textarea> <p>Another hello</p>

', 'Sent from your Twilio trial account - </textarea> <p>Another hello</p>

');
INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('56258462-4a18-4783-8b64-55fce59a152b', '2021-05-20 18:14:46.260342-07', '2021-05-20 18:14:46.260342-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-19 16:14:26.072-07', 'Wendy''s 6', 8, 'Sent from your Twilio trial account - <b>Hello world</b>

Sent from your Twilio trial account - Goober

', 'Sent from your Twilio trial account - <b>Hello world</b>

Sent from your Twilio trial account - Goober

');
INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('6b02eac6-1d5d-48d5-af42-57a4bac10783', '2021-05-20 08:48:16.463602-07', '2021-05-20 08:48:16.463602-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-19 16:15:00-07', 'Wendy''s 6', 8, 'Bent from your Twilio trial account - </textarea> <p>Another hello</p>

', 'Bent from your Twilio trial account - </textarea> <p>Another hello</p>

');
INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('982481a3-9a45-4b6b-b733-ce6d4a9f0b86', '2021-05-20 18:41:53.1156-07', '2021-05-20 18:41:53.1156-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-19 16:15:00.355-07', 'Booger', 8, 'Sent from your Twilio trial account - </textarea> <p>Another hello</p>

', 'Sent from your Twilio trial account - </textarea> <p>Another hello</p>

');
INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('156a3dc5-3212-471f-8e5e-db27e08fe635', '2021-05-20 22:18:47.725184-07', '2021-05-20 22:18:47.725184-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-19 23:27:12-07', 'Barfy', 8, 'Sent from your Twilio trial account - Goober

', 'Sent from your Twilio trial account - Goober

');
INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('a80fe09c-71a0-4e2a-9d4d-10cbda000780', '2021-05-20 23:00:57.220251-07', '2021-05-20 23:00:57.220251-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-19 23:27:12.673-07', 'Snorkle', 8, 'Sent from your Twilio trial account - Goober

', 'Sent from your Twilio trial account - Goober

');
INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('670ee52b-d425-4d9f-977a-135508ae1a9d', '2021-05-20 18:41:40.505548-07', '2021-05-20 18:41:40.505548-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-19 00:00:00-07', 'Big', 8, 'Sent from your Twilio trial account - <b>Hello world</b>

', 'Sent from your Twilio trial account - <b>Hello world</b>

');
INSERT INTO public.timecard_entries (id, created_at, updated_at, person_id, date, job_name, hours_worked, work_done, invoice_translation) VALUES ('56dfe4db-8680-45ad-9c45-5831f1680921', '2021-05-20 18:42:11.02942-07', '2021-05-20 18:42:11.02942-07', 'c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-06-16 00:00:00-07', 'Booger2', 8, 'Sent from your Twilio trial account - Goober

', 'Sent from your Twilio trial account - Goober

');


ALTER TABLE public.timecard_entries ENABLE TRIGGER ALL;


ALTER TABLE public.twilio_messages DISABLE TRIGGER ALL;

INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('5477c205-a71b-4e34-8757-c17998c9e20f', '2021-05-17 09:07:29.259572-07', '2021-05-17 09:07:29.259572-07', '2010-04-01', 'SM3c2bfb9d5c6d4fbca78ed16726bae6ec', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Goof', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('404390c0-fb94-470c-91d6-252556d03e76', '2021-05-16 02:43:19.807229-07', '2021-05-16 02:43:19.807229-07', '2010-04-01', 'SMfa400b514fe143fc8e68e7ac83ab39fb', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - What is your name?', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('25ecc109-91c1-4b9d-b342-9946893872d1', '2021-05-16 02:43:45.858398-07', '2021-05-16 02:43:45.858398-07', '2010-04-01', 'SM397c3d22461248d49f8eee4e652b031a', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Seriously', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('754d76ab-f86e-4983-a1e0-c287f630d99c', '2021-05-16 02:45:15.035178-07', '2021-05-16 02:45:15.035178-07', '2010-04-01', 'SM54738bca2f7f40b8818f26b6b05bef16', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Does status update work?', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('42ecaf60-349d-410a-9537-5f4965fd80c7', '2021-05-16 02:47:45.725897-07', '2021-05-16 02:47:45.725897-07', '2010-04-01', 'SM0985f80c14839db32c3dc5248dff6489', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'received', 'Yes!!', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('39535435-9d10-46be-8020-2724c27fa099', '2021-05-17 13:00:32.126379-07', '2021-05-17 13:00:32.126379-07', '2010-04-01', 'SM431d0d9957074debadd1784bdc7401f6', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - hello', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('425a2b91-4fd7-4faa-8b82-596cf2c6317c', '2021-05-16 02:47:54.518456-07', '2021-05-16 02:47:54.518456-07', '2010-04-01', 'SM1af52afc868543e78c31162c86ac609a', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Boom!', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('7bf96210-030b-45c0-a630-2b40552d1d09', '2021-05-16 02:48:04.633172-07', '2021-05-16 02:48:04.633172-07', '2010-04-01', 'SM403c9fe1cb6b4abab39b0c9be41c3279', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Again', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('ba94ae17-ed34-46fa-8cc2-a0fc3b934e22', '2021-05-16 20:12:20.502536-07', '2021-05-16 20:12:20.502536-07', '2010-04-01', 'SM506f96d151f3472abfe998ec2d16ad00', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Gorf', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('6f45008e-2a6d-4106-aec5-97645aba89bb', '2021-05-17 13:00:42.264301-07', '2021-05-17 13:00:42.264301-07', '2010-04-01', 'SM23486ea154704566bec5a0e13f162569', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - hello2', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('8a367e28-1135-4bcb-8a48-ba7811d06d4d', '2021-05-16 21:22:36.629437-07', '2021-05-16 21:22:36.629437-07', '2010-04-01', 'SMefd73d30093b41f6bf608f5ba0640348', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Go', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('be77f0f1-47c7-4479-8a74-775f771fae2f', '2021-05-16 21:22:55.893477-07', '2021-05-16 21:22:55.893477-07', '2010-04-01', 'SMaf7b5a81bd12445796ed6c3f10b54c39', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Now!', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('a506066c-11b8-4e6c-9bb6-cfa02c5897ac', '2021-05-16 21:25:09.620511-07', '2021-05-16 21:25:09.620511-07', '2010-04-01', 'SM044bd64b9588586811633292ba1152a7', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'received', 'Ok', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('3ce2e7a8-0d1d-4bf6-9406-e18a637c347a', '2021-05-17 21:40:10.729986-07', '2021-05-17 21:40:10.729986-07', '2010-04-01', 'SMbf91eccc8ffa4e5489eb2e782f9f91e7', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - What''s up?', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('49b66fb6-9d55-430b-a5af-4a4fab7f2666', '2021-05-16 21:27:21.579128-07', '2021-05-16 21:27:21.579128-07', '2010-04-01', 'SMe43250da582e4e1bbffd5297317725ae', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Sweet', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('698b34bb-339b-46fb-a8fd-bfdbb6ba4cf0', '2021-05-17 18:10:19.451251-07', '2021-05-17 18:10:19.451251-07', '2010-04-01', 'SM19e113b92f2d4a9b8342b2ae51a41288', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Yay!', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('4f93463e-8466-458d-bd2e-8bb116b3e65e', '2021-05-17 21:55:39.700296-07', '2021-05-17 21:55:39.700296-07', '2010-04-01', 'SMf97cb5e418334cc69cf79e68e6ef7fdd', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - zoom', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('c7ef0b02-830f-4b62-8f91-f66c8b67488e', '2021-05-17 21:22:45.277739-07', '2021-05-17 21:22:45.277739-07', '2010-04-01', 'SM3f0e62d0ae63481aa4a2adae497a3481', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Google', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('65253b91-73f4-4390-a73c-4a599441353e', '2021-05-17 21:46:03.985872-07', '2021-05-17 21:46:03.985872-07', '2010-04-01', 'SMd879370a98004947824aadc91b38e0eb', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Goober', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('87a585ef-3882-439b-b9f6-480f9b0a828f', '2021-05-17 21:22:59.78977-07', '2021-05-17 21:22:59.78977-07', '2010-04-01', 'SMa8619467be7e492e8796ed294b1eb286', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Boogle', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('28c60c33-602c-4862-bd65-7903ac3071a1', '2021-05-17 21:38:55.886977-07', '2021-05-17 21:38:55.886977-07', '2010-04-01', 'SMdf6f9cb6c535493f8f1c937ebc8441f8', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - yo!', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('11c13406-fe79-40bf-b1f2-abe10dafdafa', '2021-05-17 21:50:23.858706-07', '2021-05-17 21:50:23.858706-07', '2010-04-01', 'SM7d6d0bd4528b44fdbb91cac586a467b2', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Doober', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('ca8d5379-040b-4617-b861-5da5ea4ddbf5', '2021-05-17 21:53:57.941829-07', '2021-05-17 21:53:57.941829-07', '2010-04-01', 'SM4e4edd3f96b74b40bc4d860507af07e4', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Groom', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('5f411000-bb96-4602-9c16-103d136cfa17', '2021-05-17 22:00:25.884087-07', '2021-05-17 22:00:25.884087-07', '2010-04-01', 'SMefad330f3cf24d56b3f0144d9da5f5eb', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - dsfsdf', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('f72574f2-a05d-4fd7-9216-bfcd4a33b68c', '2021-05-17 22:13:10.491377-07', '2021-05-17 22:13:10.491377-07', '2010-04-01', 'SMd73ef28afb1f40da9b8822d79e57007b', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Goobie', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('9f4ec225-7edb-4323-bf1e-c469a9da8087', '2021-05-17 22:02:51.016837-07', '2021-05-17 22:02:51.016837-07', '2010-04-01', 'SMe9ec80339222423dbacbb96dc9cc25c1', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - goober', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('f9437a70-6e2a-4212-bf02-e0f788c3ca6e', '2021-05-17 22:13:31.758591-07', '2021-05-17 22:13:31.758591-07', '2010-04-01', 'SM47ef12d35e404b1ea419211c51859fee', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Newbie', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('d82c8f6d-5840-4f2f-9fdb-843ec1ebe0fb', '2021-05-17 22:13:43.766416-07', '2021-05-17 22:13:43.766416-07', '2010-04-01', 'SM424794fb8a9942c4bdcc7e8fa09f9136', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Zoobie', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('90593c5b-8caf-4e6a-9910-78041ad5132b', '2021-05-17 22:16:10.616908-07', '2021-05-17 22:16:10.616908-07', '2010-04-01', 'SM9677b56544994d118bfca4e18342951e', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Hello', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('8ba64c88-8b6b-4a68-96c8-b4c79349d2a2', '2021-05-17 22:17:14.741675-07', '2021-05-17 22:17:14.741675-07', '2010-04-01', 'SM9e58db4a022a42d99642043914ac1667', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Garf', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('8cf30b3d-2aa6-47d1-a0f3-d1ddde6b1be8', '2021-05-17 22:19:27.886952-07', '2021-05-17 22:19:27.886952-07', '2010-04-01', 'SM7e05c5835a6945998cdbbda75770e3b1', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - a', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('e2f72372-e724-4dd5-8d03-b83650799ffb', '2021-05-17 22:22:32.277019-07', '2021-05-17 22:22:32.277019-07', '2010-04-01', 'SMd68452b5c62c4a4c9a58052a685160a1', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Go', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('de2599eb-9250-42fc-aeee-d79e09909a03', '2021-05-17 22:29:50.808775-07', '2021-05-17 22:29:50.808775-07', '2010-04-01', 'SM99f77d04cd6e4b0aa7a075b90d0e4671', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - a', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('8b560ed6-a69b-45c0-bada-35e0eabf8a41', '2021-05-18 09:52:52.643026-07', '2021-05-18 09:52:52.643026-07', '2010-04-01', 'SMeeebc5996bc848c2a4c7dcc8b0589164', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Test', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('6aa80342-9b37-43f8-9b19-12184061cf3c', '2021-05-17 22:31:09.44107-07', '2021-05-17 22:31:09.44107-07', '2010-04-01', 'SM8eabb651b56441b085e5d0303f7f58a5', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - b', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('647303a7-f6b5-49c3-8093-1de1173da9c3', '2021-05-17 22:37:05.928741-07', '2021-05-17 22:37:05.928741-07', '2010-04-01', 'SM1311f3ca25fe470eb180faedec055636', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - hello', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('fcc0f214-4442-4696-97d7-e18e71da383d', '2021-05-17 23:04:35.683108-07', '2021-05-17 23:04:35.683108-07', '2010-04-01', 'SMa91c2332f07349329396c23fd9cd5971', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - a', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('91837fb9-665a-441c-869f-783569bba7b2', '2021-05-17 23:05:34.346871-07', '2021-05-17 23:05:34.346871-07', '2010-04-01', 'SMba5d855af59b4a108c575af32f0e87ad', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - sdf', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('ee3b991b-beb2-47d4-a7d4-748b5e11b877', '2021-05-17 23:07:56.738368-07', '2021-05-17 23:07:56.738368-07', '2010-04-01', 'SM6901f3c27ca249dc97d86d5fefaf66ed', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Blarg', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('415fcad3-4081-4241-aca9-aba759113930', '2021-05-17 23:37:30.458475-07', '2021-05-17 23:37:30.458475-07', '2010-04-01', 'SM259077f346054a0e811b6197a4227101', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Goof', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('2de7debc-6b31-41ca-89fa-8fa9daac1b55', '2021-05-18 07:59:48.865601-07', '2021-05-18 07:59:48.865601-07', '2010-04-01', 'SM79af2a7eb3ee41e0b277a9b0be9c7bd9', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Yo', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('a33437e0-7988-4c00-8f2c-a9c8c4b91566', '2021-05-18 08:49:16.686624-07', '2021-05-18 08:49:16.686624-07', '2010-04-01', 'SM2c3cead60efb4029a84e809d106eb8d0', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Yay!', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('bcb8e8e8-67dd-45ca-a033-ec21d2c60a4b', '2021-05-18 09:52:09.401323-07', '2021-05-18 09:52:09.401323-07', '2010-04-01', 'SMfb0ac20a6adb4108bf7ea8bf40aa5157', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Bargle', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('54cfcf5c-7ba9-43f6-8f31-fef126d846f1', '2021-05-18 18:44:40.959584-07', '2021-05-18 18:44:40.959584-07', '2010-04-01', 'SMb9cffd1a9f8b539dba32f773c553924f', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'received', 'Boogers
Hamburgers
Pickles', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('cf1516a6-25ac-4bb7-b7fb-81ebfed528a0', '2021-05-18 22:30:28.265057-07', '2021-05-18 22:30:28.265057-07', '2010-04-01', 'SM351c3d37000c4ba3bef8e276c9b883a7', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'aae83fea-7dd3-44eb-b51e-dbaf9a813db0', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Thank you', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('53d88b58-2440-404e-988c-5000ec8729cd', '2021-05-19 16:14:26.072438-07', '2021-05-19 16:14:26.072438-07', '2010-04-01', 'SM4bf39463981745c6884588fbd0ed78b8', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - <b>Hello world</b>', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('33607a17-9ed7-4210-a104-6902bc719d63', '2021-05-19 16:15:00.355047-07', '2021-05-19 16:15:00.355047-07', '2010-04-01', 'SMb2a59ce963f5413ca04318570098c8ca', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - </textarea> <p>Another hello</p>', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('a1a39f21-d311-4d1e-86aa-da23d178a893', '2021-05-19 23:27:12.673893-07', '2021-05-19 23:27:12.673893-07', '2010-04-01', 'SM00924232e551428fbba6349b978443ab', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Goober', 0);
INSERT INTO public.twilio_messages (id, created_at, updated_at, api_version, message_sid, account_sid, messaging_service_sid, to_id, from_id, status, body, num_media) VALUES ('a1dafd50-dc45-4a35-a7cc-713043c36b5f', '2021-05-21 00:04:53.662629-07', '2021-05-21 00:04:53.662629-07', '2010-04-01', 'SMce8e06ecd3d74dc1a37cb9dc3fa7fccf', 'AC828cf7fa609e74ef78861e56ad166f42', NULL, 'c1375981-0102-4132-8799-54f4fe3e0fbd', 'c1e2457b-60ce-4e84-8ba0-a12020c49d40', 'delivered', 'Sent from your Twilio trial account - Gorf', 0);


ALTER TABLE public.twilio_messages ENABLE TRIGGER ALL;


ALTER TABLE public.timecard_entry_messages DISABLE TRIGGER ALL;

INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('cf24e20f-5fae-48e7-ba12-695066d88d46', '2021-05-19 23:59:12.859364-07', '2021-05-19 23:59:12.859364-07', 'dc95f428-1527-4f62-9fa8-9bf8b3a8466d', '54cfcf5c-7ba9-43f6-8f31-fef126d846f1');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('11ef64f4-510a-4874-b603-506d23dd82a8', '2021-05-19 23:59:33.275019-07', '2021-05-19 23:59:33.275019-07', '9f12ff91-6eb2-421e-b6bd-c7dd4310fd5f', '647303a7-f6b5-49c3-8093-1de1173da9c3');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('2ef5b7a4-964b-46a6-886c-632ba0992924', '2021-05-20 15:28:36.527299-07', '2021-05-20 15:28:36.527299-07', '99b923ae-d69a-4238-9a8d-7ab75410cf18', '33607a17-9ed7-4210-a104-6902bc719d63');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('79094d74-bd86-4be7-8a32-03dab481e30e', '2021-05-20 15:29:07.046393-07', '2021-05-20 15:29:07.046393-07', '86b8ac98-939f-4d04-b904-d79a49ed9c31', '33607a17-9ed7-4210-a104-6902bc719d63');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('44a5ba81-ac7b-4905-b4f8-c107821e7c8c', '2021-05-20 18:14:46.262717-07', '2021-05-20 18:14:46.262717-07', '56258462-4a18-4783-8b64-55fce59a152b', '53d88b58-2440-404e-988c-5000ec8729cd');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('481987fe-5996-4c0b-ac34-dacff92e1f73', '2021-05-20 18:14:46.263546-07', '2021-05-20 18:14:46.263546-07', '56258462-4a18-4783-8b64-55fce59a152b', 'a1a39f21-d311-4d1e-86aa-da23d178a893');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('d59ad1c2-34a7-4c97-9c90-b808849164f0', '2021-05-20 18:28:18.02847-07', '2021-05-20 18:28:18.02847-07', '6b02eac6-1d5d-48d5-af42-57a4bac10783', '33607a17-9ed7-4210-a104-6902bc719d63');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('68ab9eb0-69cf-4365-9c8d-793e1099f15b', '2021-05-20 18:41:53.117893-07', '2021-05-20 18:41:53.117893-07', '982481a3-9a45-4b6b-b733-ce6d4a9f0b86', '33607a17-9ed7-4210-a104-6902bc719d63');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('1c8407ab-de94-4f0a-93d7-0e4d618bb084', '2021-05-20 22:18:47.735371-07', '2021-05-20 22:18:47.735371-07', '156a3dc5-3212-471f-8e5e-db27e08fe635', 'a1a39f21-d311-4d1e-86aa-da23d178a893');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('df7ec4fd-83d4-4ade-afea-f780c35282fc', '2021-05-20 23:00:57.223565-07', '2021-05-20 23:00:57.223565-07', 'a80fe09c-71a0-4e2a-9d4d-10cbda000780', 'a1a39f21-d311-4d1e-86aa-da23d178a893');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('25b069c6-f0d2-4092-adb1-f6f7b05c4166', '2021-05-20 23:19:21.524854-07', '2021-05-20 23:19:21.524854-07', '670ee52b-d425-4d9f-977a-135508ae1a9d', 'a1a39f21-d311-4d1e-86aa-da23d178a893');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('3e124d61-ff76-4aec-ac50-98ee74a04947', '2021-05-21 11:58:35.342225-07', '2021-05-21 11:58:35.342225-07', '56dfe4db-8680-45ad-9c45-5831f1680921', '33607a17-9ed7-4210-a104-6902bc719d63');
INSERT INTO public.timecard_entry_messages (id, created_at, updated_at, timecard_entry_id, twilio_message_id) VALUES ('61733f79-7c87-4995-b815-f58ac7be55a9', '2021-05-21 11:58:35.342225-07', '2021-05-21 11:58:35.342225-07', '56dfe4db-8680-45ad-9c45-5831f1680921', 'a1dafd50-dc45-4a35-a7cc-713043c36b5f');


ALTER TABLE public.timecard_entry_messages ENABLE TRIGGER ALL;


ALTER TABLE public.users DISABLE TRIGGER ALL;

INSERT INTO public.users (id, email, password_hash, locked_at, failed_login_attempts) VALUES ('aa267118-f73f-45ab-9644-5705f21070e7', 'test@company.com', 'sha256|17|V6JGPzE2wOfao5X6hI+QPA==|5/99LLTnfGArcvfziM8eIvlOMN/syoKpKvvW3NaRBvM=', NULL, 0);


ALTER TABLE public.users ENABLE TRIGGER ALL;


