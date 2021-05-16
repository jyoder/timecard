

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

ALTER TABLE public.persons DISABLE TRIGGER ALL;

INSERT INTO public.persons (id, created_at, updated_at, first_name, last_name, goes_by) VALUES ('5419fec4-1380-475b-a6e7-87f1ea8870a6', '2021-05-11 22:58:57.614427-07', '2021-05-11 22:58:57.614427-07', 'Tim', 'Eckard', 'Tim the Bot');
INSERT INTO public.persons (id, created_at, updated_at, first_name, last_name, goes_by) VALUES ('4383aa8e-d200-45cb-9c8e-0090f14457ef', '2021-05-11 22:07:30.127923-07', '2021-05-11 22:07:30.127923-07', 'Emma', 'Nazim', 'Emma');
INSERT INTO public.persons (id, created_at, updated_at, first_name, last_name, goes_by) VALUES ('cfce1f7b-7617-4c5d-8349-e6640378e01e', '2021-05-12 23:36:10.535243-07', '2021-05-12 23:36:10.535243-07', 'Molly', 'Abbott', 'Molly');
INSERT INTO public.persons (id, created_at, updated_at, first_name, last_name, goes_by) VALUES ('c8a20c60-1c95-4a9e-bb6a-57ca2670115d', '2021-05-14 14:09:54.579756-07', '2021-05-14 14:09:54.579756-07', 'John', 'Yoder', 'John');
INSERT INTO public.persons (id, created_at, updated_at, first_name, last_name, goes_by) VALUES ('6e41e1a0-7f59-4195-ad39-546fcf1e6b79', '2021-05-14 14:10:03.581771-07', '2021-05-14 14:10:03.581771-07', 'Hillary', 'Yoder', 'Hillary');


ALTER TABLE public.persons ENABLE TRIGGER ALL;


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


ALTER TABLE public.phone_messages DISABLE TRIGGER ALL;


ALTER TABLE public.phone_messages ENABLE TRIGGER ALL;


ALTER TABLE public.twilio_geo_message_details DISABLE TRIGGER ALL;



ALTER TABLE public.twilio_geo_message_details ENABLE TRIGGER ALL;


ALTER TABLE public.twilio_messages DISABLE TRIGGER ALL;



ALTER TABLE public.twilio_messages ENABLE TRIGGER ALL;


