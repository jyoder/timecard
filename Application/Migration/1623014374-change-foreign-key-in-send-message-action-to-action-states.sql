ALTER TABLE send_message_actions DROP CONSTRAINT send_message_actions_ref_action_run_time_id;
DROP INDEX send_message_actions_action_run_time_id_index;
ALTER TABLE send_message_actions DROP COLUMN action_run_time_id;

ALTER TABLE send_message_actions ADD COLUMN action_run_state_id UUID NOT NULL;
CREATE INDEX send_message_actions_action_run_state_id_index ON send_message_actions (action_run_state_id);
ALTER TABLE send_message_actions ADD CONSTRAINT send_message_actions_ref_action_run_state_id FOREIGN KEY (action_run_state_id) REFERENCES action_run_states (id) ON DELETE NO ACTION;
