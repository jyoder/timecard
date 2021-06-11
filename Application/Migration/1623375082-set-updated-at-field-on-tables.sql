CREATE FUNCTION trigger_set_updated_at()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ language plpgsql;

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
