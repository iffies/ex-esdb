-module(configs).

-export([setup_tracking/2]).

setup_tracking(Store, PidOrPids) ->
  tracker_group:join(Store, configs, PidOrPids),
  configs_procs:put_on_create_func(Store),
  configs_triggers:register_on_created(Store),
  configs_procs:put_on_delete_func(Store),
  configs_triggers:register_on_deleted(Store),
  configs_procs:put_on_update_func(Store),
  configs_triggers:register_on_updated(Store),
  ok.
