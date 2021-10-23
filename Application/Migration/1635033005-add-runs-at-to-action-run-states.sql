-- Write your SQL migration code in here
-- This file is created by the IHP Schema Designer.
-- When you generate a new migration, all changes in this file will be copied into your migration.
-- -- Use http://localhost:8001/NewMigration or `new-migration` to generate a new migration.
-- -- Learn more about migrations: https://ihp.digitallyinduced.com/Guide/database-migrations.html
alter table action_run_states
    add column runs_at TIMESTAMP WITH TIME ZONE default now() not null;

update
    action_run_states
set
    runs_at = action_run_times.runs_at
from
    action_run_times
where
    action_run_times.action_run_state_id = action_run_states.id;

