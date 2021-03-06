-- pgFormatter-ignore
-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TYPE audit_actions AS ENUM ('message_sent', 'message_received', 'message_processed', 'timecard_entry_created', 'timecard_entry_edited', 'timecard_entry_deleted', 'review_link_generated', 'review_signed', 'daily_reminder_scheduled', 'review_request_scheduled', 'scheduled_message_edited', 'scheduled_message_suspended', 'scheduled_message_resumed', 'scheduled_message_canceled');
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    CHECK (trim(email) <> ''),
    CHECK (trim(password_hash) <> '')
);
CREATE TABLE people (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    goes_by TEXT NOT NULL,
    CHECK (trim(first_name) <> ''),
    CHECK (trim(last_name) <> ''),
    CHECK (trim(goes_by) <> '')
);
CREATE TABLE phone_numbers (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    number TEXT NOT NULL UNIQUE
);
CREATE TABLE phone_contacts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    person_id UUID NOT NULL UNIQUE,
    phone_number_id UUID NOT NULL UNIQUE
);
CREATE TABLE twilio_messages (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    api_version TEXT NOT NULL,
    message_sid TEXT NOT NULL UNIQUE,
    account_sid TEXT NOT NULL,
    messaging_service_sid TEXT,
    to_id UUID NOT NULL,
    from_id UUID NOT NULL,
    status TEXT NOT NULL,
    body TEXT NOT NULL,
    num_media INT NOT NULL
);
CREATE TABLE timecard_entry_messages (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    timecard_entry_id UUID NOT NULL,
    twilio_message_id UUID NOT NULL,
    UNIQUE(timecard_entry_id, twilio_message_id)
);
CREATE TABLE timecard_entries (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    date DATE NOT NULL,
    job_name TEXT NOT NULL,
    hours_worked DOUBLE PRECISION NOT NULL,
    work_done TEXT NOT NULL,
    invoice_translation TEXT NOT NULL,
    timecard_id UUID NOT NULL,
    clocked_in_at TIME DEFAULT NULL,
    clocked_out_at TIME DEFAULT NULL,
    lunch_duration INT DEFAULT NULL,
    CHECK (trim(job_name) <> ''),
    CHECK (trim(work_done) <> ''),
    CHECK (trim(invoice_translation) <> '')
);
CREATE TABLE process_events_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE TABLE send_message_actions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    body TEXT NOT NULL,
    from_id UUID DEFAULT uuid_generate_v4() NOT NULL,
    to_id UUID DEFAULT uuid_generate_v4() NOT NULL,
    action_run_state_id UUID NOT NULL,
    CHECK (trim(body) <> '')
);
CREATE TABLE action_run_states (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    state TEXT DEFAULT 'not_started' NOT NULL,
    runs_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    CHECK (((((state = 'not_started' OR state = 'suspended') OR state = 'running') OR state = 'canceled') OR state = 'finished') OR state = 'failed')
);
CREATE TABLE timecards (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    week_of DATE NOT NULL,
    person_id UUID NOT NULL,
    UNIQUE(person_id, week_of)
);
CREATE TABLE access_tokens (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    value TEXT NOT NULL,
    expires_at TIMESTAMP WITH TIME ZONE NOT NULL,
    is_revoked BOOLEAN DEFAULT false NOT NULL,
    CHECK (trim(value) <> '')
);
CREATE TABLE timecard_access_tokens (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    timecard_id UUID NOT NULL,
    access_token_id UUID NOT NULL,
    UNIQUE(access_token_id)
);
CREATE TABLE signings (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    signed_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    name TEXT NOT NULL,
    ip_address TEXT NOT NULL,
    CHECK (trim(name) <> ''),
    CHECK (trim(ip_address) <> '')
);
CREATE TABLE timecard_signings (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    timecard_id UUID NOT NULL,
    signing_id UUID NOT NULL,
    UNIQUE(timecard_id, signing_id)
);
CREATE TABLE worker_settings (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    person_id UUID NOT NULL,
    send_daily_reminder_at TIME NOT NULL,
    is_active BOOLEAN DEFAULT true NOT NULL,
    preferred_language TEXT DEFAULT 'english' NOT NULL,
    CHECK (preferred_language = 'english' OR preferred_language = 'spanish')
);
CREATE TABLE vertex_ai_entity_predictions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    vertex_ai_id TEXT NOT NULL,
    display_name TEXT NOT NULL,
    segment_start_offset INT NOT NULL,
    segment_end_offset INT NOT NULL,
    confidence DOUBLE PRECISION NOT NULL,
    deployed_model_id TEXT NOT NULL,
    CHECK (trim(vertex_ai_id) <> ''),
    CHECK (trim(display_name) <> ''),
    CHECK (segment_start_offset >= 0),
    CHECK (segment_end_offset >= segment_start_offset),
    CHECK (confidence >= 0.0),
    CHECK (confidence <= 1.0)
);
CREATE TABLE twilio_message_entities (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    twilio_message_id UUID NOT NULL,
    vertex_ai_entity_prediction_id UUID NOT NULL,
    UNIQUE(twilio_message_id, vertex_ai_entity_prediction_id)
);
CREATE TABLE fetch_entity_prediction_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    twilio_message_id UUID NOT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE TABLE audit_entries (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    phone_number_id UUID NOT NULL,
    user_id UUID,
    "action" audit_actions NOT NULL,
    action_context TEXT NOT NULL
);
CREATE FUNCTION trigger_validate_timecard_entry_date_matches_timecard() RETURNS TRIGGER AS $$
BEGIN
  IF date_trunc('week', NEW.date) = (SELECT timecards.week_of FROM timecards WHERE timecards.id = NEW.timecard_id) THEN
    RETURN NEW;
  ELSE
    RAISE EXCEPTION 'date must fall within the timecard week';
  END IF;
END;
$$ language plpgsql;
CREATE TRIGGER validate_timecard_entry_date_matches_timecard BEFORE INSERT OR UPDATE ON timecard_entries FOR EACH ROW EXECUTE PROCEDURE trigger_validate_timecard_entry_date_matches_timecard();
CREATE FUNCTION trigger_validate_timecard_week_of_matches_timecard_entries() RETURNS TRIGGER AS $$
DECLARE
  week_of DATE;
BEGIN
  week_of := (SELECT date_trunc('week', timecard_entries.date)::date week_of FROM timecard_entries WHERE timecard_entries.timecard_id = NEW.id limit 1);
  IF week_of IS NULL OR week_of = NEW.week_of THEN
    RETURN NEW;
  ELSE
    RAISE EXCEPTION 'week_of must encompass timecard entry dates';
  END IF;
END;
$$ language plpgsql;
CREATE TRIGGER validate_timecard_week_of_matches_timecard_entries BEFORE INSERT OR UPDATE ON timecards FOR EACH ROW EXECUTE PROCEDURE trigger_validate_timecard_week_of_matches_timecard_entries();
CREATE FUNCTION trigger_validate_timecard_week_of_is_start_of_week() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.week_of = date_trunc('week', NEW.week_of)::date THEN
    RETURN NEW;
  ELSE
    RAISE EXCEPTION 'week_of must be the start of the week';
  END IF;
END;
$$ language plpgsql;
CREATE TRIGGER validate_timecard_week_of_is_start_of_week BEFORE INSERT OR UPDATE ON timecards FOR EACH ROW EXECUTE PROCEDURE trigger_validate_timecard_week_of_is_start_of_week();
CREATE INDEX action_run_states_runs_at_index ON action_run_states (runs_at);
CREATE INDEX phone_contacts_person_id_index ON phone_contacts (person_id);
CREATE INDEX phone_contacts_phone_number_id_index ON phone_contacts (phone_number_id);
CREATE INDEX send_message_actions_from_id_index ON send_message_actions (from_id);
CREATE INDEX send_message_actions_to_id_index ON send_message_actions (to_id);
CREATE INDEX timecard_entry_messages_timecard_entry_id_index ON timecard_entry_messages (timecard_entry_id);
CREATE INDEX timecard_entry_messages_twilio_message_id_index ON timecard_entry_messages (twilio_message_id);
CREATE INDEX twilio_messages_from_id_index ON twilio_messages (from_id);
CREATE INDEX twilio_messages_to_id_index ON twilio_messages (to_id);
CREATE INDEX send_message_actions_action_run_state_id_index ON send_message_actions (action_run_state_id);
CREATE INDEX action_run_states_state_index ON action_run_states (state);
CREATE INDEX timecard_entries_timecard_id_index ON timecard_entries (timecard_id);
CREATE INDEX timecards_person_id_index ON timecards (person_id);
CREATE INDEX timecard_access_tokens_timecard_id_index ON timecard_access_tokens (timecard_id);
CREATE INDEX timecard_access_tokens_access_token_id_index ON timecard_access_tokens (access_token_id);
CREATE INDEX timecard_signings_timecard_id_index ON timecard_signings (timecard_id);
CREATE INDEX timecard_signings_signing_id_index ON timecard_signings (signing_id);
CREATE INDEX worker_settings_person_id_index ON worker_settings (person_id);
CREATE INDEX twilio_message_entities_twilio_message_id_index ON twilio_message_entities (twilio_message_id);
CREATE INDEX twilio_message_entities_vertex_ai_entity_prediction_id_index ON twilio_message_entities (vertex_ai_entity_prediction_id);
CREATE INDEX fetch_entity_prediction_jobs_twilio_message_id_index ON fetch_entity_prediction_jobs (twilio_message_id);
CREATE INDEX audit_entries_phone_number_id_index ON audit_entries (phone_number_id);
CREATE INDEX audit_entries_user_id_index ON audit_entries (user_id);
CREATE FUNCTION trigger_set_updated_at() RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ language plpgsql;
CREATE TRIGGER set_updated_at BEFORE UPDATE ON users FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON people FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON phone_numbers FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON phone_contacts FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON twilio_messages FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON timecard_entry_messages FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON timecard_entries FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON process_events_jobs FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON send_message_actions FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON action_run_states FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON timecards FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON access_tokens FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON timecard_access_tokens FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON signings FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON timecard_signings FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON worker_settings FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON vertex_ai_entity_predictions FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON twilio_message_entities FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON fetch_entity_prediction_jobs FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON audit_entries FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
ALTER TABLE audit_entries ADD CONSTRAINT audit_entries_ref_phone_number_id FOREIGN KEY (phone_number_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE audit_entries ADD CONSTRAINT audit_entries_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
ALTER TABLE fetch_entity_prediction_jobs ADD CONSTRAINT fetch_entity_prediction_jobs_ref_twilio_message_id FOREIGN KEY (twilio_message_id) REFERENCES twilio_messages (id) ON DELETE NO ACTION;
ALTER TABLE phone_contacts ADD CONSTRAINT phone_contacts_ref_person_id FOREIGN KEY (person_id) REFERENCES people (id) ON DELETE NO ACTION;
ALTER TABLE phone_contacts ADD CONSTRAINT phone_contacts_ref_phone_number_id FOREIGN KEY (phone_number_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE send_message_actions ADD CONSTRAINT send_message_actions_ref_action_run_state_id FOREIGN KEY (action_run_state_id) REFERENCES action_run_states (id) ON DELETE NO ACTION;
ALTER TABLE send_message_actions ADD CONSTRAINT send_message_actions_ref_from_id FOREIGN KEY (from_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE send_message_actions ADD CONSTRAINT send_message_actions_ref_to_id FOREIGN KEY (to_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE timecard_access_tokens ADD CONSTRAINT timecard_access_tokens_ref_access_token_id FOREIGN KEY (access_token_id) REFERENCES access_tokens (id) ON DELETE NO ACTION;
ALTER TABLE timecard_access_tokens ADD CONSTRAINT timecard_access_tokens_ref_timecard_id FOREIGN KEY (timecard_id) REFERENCES timecards (id) ON DELETE NO ACTION;
ALTER TABLE timecard_entries ADD CONSTRAINT timecard_entries_ref_timecard_id FOREIGN KEY (timecard_id) REFERENCES timecards (id) ON DELETE NO ACTION;
ALTER TABLE timecard_entry_messages ADD CONSTRAINT timecard_entry_messages_ref_timecard_entry_id FOREIGN KEY (timecard_entry_id) REFERENCES timecard_entries (id) ON DELETE NO ACTION;
ALTER TABLE timecard_entry_messages ADD CONSTRAINT timecard_entry_messages_ref_twilio_message_id FOREIGN KEY (twilio_message_id) REFERENCES twilio_messages (id) ON DELETE NO ACTION;
ALTER TABLE timecard_signings ADD CONSTRAINT timecard_signings_ref_signing_id FOREIGN KEY (signing_id) REFERENCES signings (id) ON DELETE NO ACTION;
ALTER TABLE timecard_signings ADD CONSTRAINT timecard_signings_ref_timecard_id FOREIGN KEY (timecard_id) REFERENCES timecards (id) ON DELETE NO ACTION;
ALTER TABLE timecards ADD CONSTRAINT timecards_ref_person_id FOREIGN KEY (person_id) REFERENCES people (id) ON DELETE NO ACTION;
ALTER TABLE twilio_message_entities ADD CONSTRAINT twilio_message_entities_ref_twilio_message_id FOREIGN KEY (twilio_message_id) REFERENCES twilio_messages (id) ON DELETE NO ACTION;
ALTER TABLE twilio_message_entities ADD CONSTRAINT twilio_message_entities_ref_vertex_ai_entity_prediction_id FOREIGN KEY (vertex_ai_entity_prediction_id) REFERENCES vertex_ai_entity_predictions (id) ON DELETE NO ACTION;
ALTER TABLE twilio_messages ADD CONSTRAINT twilio_messages_ref_from_id FOREIGN KEY (from_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE twilio_messages ADD CONSTRAINT twilio_messages_ref_to_id FOREIGN KEY (to_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE worker_settings ADD CONSTRAINT worker_settings_ref_person_id FOREIGN KEY (person_id) REFERENCES people (id) ON DELETE NO ACTION;
