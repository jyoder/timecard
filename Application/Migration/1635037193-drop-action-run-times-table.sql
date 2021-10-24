-- Write your SQL migration code in here
-- This file is created by the IHP Schema Designer.
-- When you generate a new migration, all changes in this file will be copied into your migration.
-- -- Use http://localhost:8001/NewMigration or `new-migration` to generate a new migration.
-- -- Learn more about migrations: https://ihp.digitallyinduced.com/Guide/database-migrations.html
update
    action_run_states
set
    runs_at = action_run_times.runs_at
from
    action_run_times
where
    action_run_times.action_run_state_id = action_run_states.id;

drop trigger if exists set_updated_at on action_run_times;

alter table action_run_times
    drop constraint action_run_times_ref_action_run_state_id;

alter table action_run_times
    drop constraint action_run_times_pkey;

drop index if exists action_run_times_action_run_state_id_index;

drop index if exists action_run_times_pkey;

drop index if exists action_run_times_runs_at_index;

drop table action_run_times;

create index action_run_states_runs_at_index on action_run_states using btree (runs_at);

