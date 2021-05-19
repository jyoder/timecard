-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE persons (
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
CREATE TABLE timecard_day_entries (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    date TIMESTAMP WITH TIME ZONE NOT NULL
);
CREATE TABLE timecard_entry_messages (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    timecard_entry_id UUID NOT NULL,
    twilio_message_id UUID NOT NULL,
    UNIQUE(timecard_entry_id, twilio_message_id)
);
CREATE INDEX timecard_entry_messages_timecard_entry_id_index ON timecard_entry_messages (timecard_entry_id);
CREATE INDEX timecard_entry_messages_twilio_message_id_index ON timecard_entry_messages (twilio_message_id);
CREATE TABLE timecard_job_entries (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    timecard_entry_id UUID NOT NULL,
    job_name TEXT NOT NULL,
    hours_worked DOUBLE PRECISION NOT NULL,
    work_done TEXT NOT NULL,
    invoice_translation TEXT NOT NULL,
    date TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX timecard_job_entries_timecard_entry_id_index ON timecard_job_entries (timecard_entry_id);
ALTER TABLE phone_contacts ADD CONSTRAINT phone_contacts_ref_person_id FOREIGN KEY (person_id) REFERENCES persons (id) ON DELETE NO ACTION;
ALTER TABLE phone_contacts ADD CONSTRAINT phone_contacts_ref_phone_number_id FOREIGN KEY (phone_number_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE timecard_entry_messages ADD CONSTRAINT timecard_entry_messages_ref_timecard_entry_id FOREIGN KEY (timecard_entry_id) REFERENCES timecard_day_entries (id) ON DELETE NO ACTION;
ALTER TABLE timecard_entry_messages ADD CONSTRAINT timecard_entry_messages_ref_twilio_message_id FOREIGN KEY (twilio_message_id) REFERENCES twilio_messages (id) ON DELETE NO ACTION;
ALTER TABLE timecard_job_entries ADD CONSTRAINT timecard_job_entries_ref_timecard_entry_id FOREIGN KEY (timecard_entry_id) REFERENCES timecard_day_entries (id) ON DELETE NO ACTION;
ALTER TABLE twilio_messages ADD CONSTRAINT twilio_messages_ref_from_id FOREIGN KEY (from_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE twilio_messages ADD CONSTRAINT twilio_messages_ref_to_id FOREIGN KEY (to_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
