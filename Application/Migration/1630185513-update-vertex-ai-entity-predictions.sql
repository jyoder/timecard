alter table "public"."vertex_ai_entity_predictions"
    add column "deployed_model_id" text not null;

alter table "public"."vertex_ai_entity_predictions"
    drop constraint "vertex_ai_entity_predictions_display_name_check";

alter table "public"."vertex_ai_entity_predictions"
    drop constraint "vertex_ai_entity_predictions_vertex_ai_id_check";

alter table "public"."vertex_ai_entity_predictions"
    add constraint "vertex_ai_entity_predictions_display_name_check" check ((btrim(display_name) <> ''::text));

alter table "public"."vertex_ai_entity_predictions"
    add constraint "vertex_ai_entity_predictions_vertex_ai_id_check" check ((btrim(vertex_ai_id) <> ''::text));

