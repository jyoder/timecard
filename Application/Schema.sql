-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE phone_messages (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    to_id UUID DEFAULT uuid_generate_v4() NOT NULL,
    from_id UUID DEFAULT uuid_generate_v4() NOT NULL,
    sent_at TIMESTAMP WITH TIME ZONE,
    body TEXT NOT NULL
);
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
ALTER TABLE phone_contacts ADD CONSTRAINT phone_contacts_ref_person_id FOREIGN KEY (person_id) REFERENCES persons (id) ON DELETE NO ACTION;
ALTER TABLE phone_contacts ADD CONSTRAINT phone_contacts_ref_phone_number_id FOREIGN KEY (phone_number_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE phone_messages ADD CONSTRAINT phone_messages_ref_from_id FOREIGN KEY (from_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
ALTER TABLE phone_messages ADD CONSTRAINT phone_messages_ref_to_id FOREIGN KEY (to_id) REFERENCES phone_numbers (id) ON DELETE NO ACTION;
