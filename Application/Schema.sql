-- pgFormatter-ignore
-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE TABLE people (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    goes_by TEXT NOT NULL
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
    timecard_id UUID NOT NULL
);
CREATE TABLE process_events_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL
);
CREATE TABLE action_run_times (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    runs_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    action_run_state_id UUID NOT NULL
);
CREATE TABLE send_message_actions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    body TEXT NOT NULL,
    from_id UUID DEFAULT uuid_generate_v4() NOT NULL,
    to_id UUID DEFAULT uuid_generate_v4() NOT NULL,
    action_run_state_id UUID NOT NULL
);
CREATE TABLE action_run_states (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    state TEXT DEFAULT 'not_started' NOT NULL,
    CHECK (((((state = 'not_started') OR (state = 'running')) OR (state = 'canceled')) OR (state = 'finished')) OR (state = 'failed'))
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
    is_revoked BOOLEAN DEFAULT false NOT NULL
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
    ip_address TEXT NOT NULL
);
CREATE TABLE timecard_signings (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    timecard_id UUID NOT NULL,
    signing_id UUID NOT NULL,
    UNIQUE(timecard_id, signing_id)
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
CREATE INDEX action_run_times_runs_at_index ON action_run_times (runs_at);
CREATE INDEX phone_contacts_person_id_index ON phone_contacts (person_id);
CREATE INDEX phone_contacts_phone_number_id_index ON phone_contacts (phone_number_id);
CREATE INDEX send_message_actions_from_id_index ON send_message_actions (from_id);
CREATE INDEX send_message_actions_to_id_index ON send_message_actions (to_id);
CREATE INDEX timecard_entry_messages_timecard_entry_id_index ON timecard_entry_messages (timecard_entry_id);
CREATE INDEX timecard_entry_messages_twilio_message_id_index ON timecard_entry_messages (twilio_message_id);
CREATE INDEX twilio_messages_from_id_index ON twilio_messages (from_id);
CREATE INDEX twilio_messages_to_id_index ON twilio_messages (to_id);
CREATE INDEX action_run_times_action_run_state_id_index ON action_run_times (action_run_state_id);
CREATE INDEX send_message_actions_action_run_state_id_index ON send_message_actions (action_run_state_id);
CREATE INDEX action_run_states_state_index ON action_run_states (state);
CREATE INDEX timecard_entries_timecard_id_index ON timecard_entries (timecard_id);
CREATE INDEX timecards_person_id_index ON timecards (person_id);
CREATE INDEX timecard_access_tokens_timecard_id_index ON timecard_access_tokens (timecard_id);
CREATE INDEX timecard_access_tokens_access_token_id_index ON timecard_access_tokens (access_token_id);
CREATE INDEX timecard_signings_timecard_id_index ON timecard_signings (timecard_id);
CREATE INDEX timecard_signings_signing_id_index ON timecard_signings (signing_id);
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
CREATE TRIGGER set_updated_at BEFORE UPDATE ON action_run_times FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON send_message_actions FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON action_run_states FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON timecards FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON access_tokens FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();
CREATE TRIGGER set_updated_at BEFORE UPDATE ON timecard_access_tokens FOR EACH ROW EXECUTE PROCEDURE trigger_set_updated_at();

ALTER TABLE action_run_times ADD CONSTRAINT action_run_times_ref_action_run_state_id FOREIGN KEY (action_run_state_id) REFERENCES action_run_states (id) ON DELETE NO ACTION;
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
ALTER TABLE twilio_messages ADD CONSTRAINT twilio_messages_ref_from_id FOREIGN KEY (from_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE twilio_messages ADD CONSTRAINT twilio_messages_ref_to_id FOREIGN KEY (to_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
