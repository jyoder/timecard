-- Write your SQL migration code in here
alter table process_events_jobs
    add column run_at TIMESTAMP WITH TIME ZONE default now() not null;

update
    process_events_jobs
set
    run_at = created_at;

alter table fetch_entity_prediction_jobs
    add column run_at TIMESTAMP WITH TIME ZONE default now() not null;

update
    fetch_entity_prediction_jobs
set
    run_at = created_at;

