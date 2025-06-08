-module(streams_triggers).

-export([register_on_new_event/3]).

-spec register_on_new_event(Store :: khepri:store(),
                            Id :: string(),
                            Filter :: khepri:filter()) ->
                             ok | {error, term()}.
register_on_new_event(Store, Id, Filter) ->
  Topic = emitter_group:topic(Store, Id),
  PropOpts =
    #{expect_specific_node => false,
      props_to_return =>
        [payload, payload_version, child_list_version, child_list_length, child_names],
      include_root_props => true},
  io:puts("Registering [procs, on_new_event, ~s] FILTER: ~p~n", [Topic, Filter]),
  khepri:register_trigger(Store, Id, Filter, [procs, on_new_event, Topic], PropOpts).
