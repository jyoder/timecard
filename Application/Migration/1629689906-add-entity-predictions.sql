create table "public"."twilio_message_entities" (
    "id" uuid not null default uuid_generate_v4 (),
    "created_at" timestamp with time zone not null default now(),
    "updated_at" timestamp with time zone not null default now(),
    "twilio_message_id" uuid not null,
    "vertex_ai_entity_prediction_id" uuid not null
);

create table "public"."vertex_ai_entity_predictions" (
    "id" uuid not null default uuid_generate_v4 (),
    "created_at" timestamp with time zone not null default now(),
    "updated_at" timestamp with time zone not null default now(),
    "vertex_ai_id" text not null,
    "display_name" text not null,
    "segment_start_offset" integer not null,
    "segment_end_offset" integer not null,
    "confidence" double precision not null
);

create unique index twilio_message_entities_pkey on public.twilio_message_entities using btree (id);

create index twilio_message_entities_twilio_message_id_index on public.twilio_message_entities using btree (twilio_message_id);

create unique index twilio_message_entities_twilio_message_id_vertex_ai_entity__key on public.twilio_message_entities using btree (twilio_message_id, vertex_ai_entity_prediction_id);

create index twilio_message_entities_vertex_ai_entity_prediction_id_index on public.twilio_message_entities using btree (vertex_ai_entity_prediction_id);

create unique index vertex_ai_entity_predictions_pkey on public.vertex_ai_entity_predictions using btree (id);

alter table "public"."twilio_message_entities"
    add constraint "twilio_message_entities_pkey" primary key using index "twilio_message_entities_pkey";

alter table "public"."vertex_ai_entity_predictions"
    add constraint "vertex_ai_entity_predictions_pkey" primary key using index "vertex_ai_entity_predictions_pkey";

alter table "public"."twilio_message_entities"
    add constraint "twilio_message_entities_ref_twilio_message_id" foreign key (twilio_message_id) references twilio_messages (id);

alter table "public"."twilio_message_entities"
    add constraint "twilio_message_entities_ref_vertex_ai_entity_prediction_id" foreign key (vertex_ai_entity_prediction_id) references vertex_ai_entity_predictions (id);

alter table "public"."twilio_message_entities"
    add constraint "twilio_message_entities_twilio_message_id_vertex_ai_entity__key" unique using index "twilio_message_entities_twilio_message_id_vertex_ai_entity__key";

alter table "public"."vertex_ai_entity_predictions"
    add constraint "vertex_ai_entity_predictions_check" check ((segment_end_offset >= segment_start_offset));

alter table "public"."vertex_ai_entity_predictions"
    add constraint "vertex_ai_entity_predictions_confidence_check" check ((confidence >= (0.0)::double precision));

alter table "public"."vertex_ai_entity_predictions"
    add constraint "vertex_ai_entity_predictions_confidence_check1" check ((confidence <= (1.0)::double precision));

alter table "public"."vertex_ai_entity_predictions"
    add constraint "vertex_ai_entity_predictions_display_name_check" check (((display_name <> ''::text) is not true));

alter table "public"."vertex_ai_entity_predictions"
    add constraint "vertex_ai_entity_predictions_segment_start_offset_check" check ((segment_start_offset >= 0));

alter table "public"."vertex_ai_entity_predictions"
    add constraint "vertex_ai_entity_predictions_vertex_ai_id_check" check (((vertex_ai_id <> ''::text) is not true));

create trigger set_updated_at
    before update on public.twilio_message_entities for each row
    execute function trigger_set_updated_at ();

create trigger set_updated_at
    before update on public.vertex_ai_entity_predictions for each row
    execute function trigger_set_updated_at ();

