ALTER TABLE action_run_times ADD COLUMN action_run_state_id UUID NOT NULL
CREATE INDEX action_run_times_action_run_state_id_index ON action_run_times (action_run_state_id);
ALTER TABLE action_run_times ADD CONSTRAINT action_run_times_ref_action_run_state_id FOREIGN KEY (action_run_state_id) REFERENCES action_run_states (id) ON DELETE NO ACTION;
