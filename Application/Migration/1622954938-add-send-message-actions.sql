CREATE TABLE send_message_actions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    action_run_time_id UUID NOT NULL,
    body TEXT NOT NULL,
    from_id UUID DEFAULT uuid_generate_v4() NOT NULL,
    to_id UUID DEFAULT uuid_generate_v4() NOT NULL
);

CREATE INDEX send_message_actions_action_run_time_id_index ON send_message_actions (action_run_time_id);
CREATE INDEX send_message_actions_from_id_index ON twilio_messages (from_id);
CREATE INDEX send_message_actions_to_id_index ON twilio_messages (to_id);
