

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



ALTER TABLE public.persons ENABLE TRIGGER ALL;


ALTER TABLE public.phone_numbers DISABLE TRIGGER ALL;



ALTER TABLE public.phone_numbers ENABLE TRIGGER ALL;


ALTER TABLE public.phone_contacts DISABLE TRIGGER ALL;



ALTER TABLE public.phone_contacts ENABLE TRIGGER ALL;


ALTER TABLE public.phone_messages DISABLE TRIGGER ALL;



ALTER TABLE public.phone_messages ENABLE TRIGGER ALL;


