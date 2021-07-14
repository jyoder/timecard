alter table "action_run_states"
    drop constraint "action_run_states_state_check";

alter table "action_run_states"
    add constraint "action_run_states_state_check" check (((state = 'not_started'::text) or (state = 'suspended'::text) or (state = 'running'::text) or (state = 'canceled'::text) or (state = 'finished'::text) or (state = 'failed'::text)));

