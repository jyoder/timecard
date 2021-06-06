DROP INDEX send_message_actions_from_id_index;
DROP INDEX send_message_actions_to_id_index;

CREATE INDEX send_message_actions_from_id_index ON send_message_actions (from_id);
CREATE INDEX send_message_actions_to_id_index ON send_message_actions (to_id);