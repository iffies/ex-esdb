-module(ex_esdb_triggers).

-export([setup_emitters/4, get_on_new_event/2]).

-spec get_on_new_event(Store :: khepri:store(), Id :: string()) -> ok | {error, term()}.
get_on_new_event(Store, Id) when is_atom(Store) ->
  Topic = emitter_group:topic(Store, Id),
  khepri:get(Store, [procs, on_new_event, Topic]).

-spec put_on_new_event(Store :: khepri:store(), Id :: string()) -> ok | {error, term()}.
put_on_new_event(Store, Id) when is_atom(Store) ->
  Topic = emitter_group:topic(Store, Id),
  case khepri:exists(Store, [procs, on_new_event, Topic]) of
    true ->
      ok;
    false ->
      ok =
        khepri:put(Store,
                   [procs, on_new_event, Topic],
                   fun(Props) ->
                      case maps:get(path, Props, undefined) of
                        undefined -> ok;
                        Path ->
                          case get_event(Store, Path) of
                            {ok, undefined} -> ok;
                            {ok, Event} ->
                              emitter_group:broadcast(Store, Id, Event),
                              ok;
                            {error, Reason} ->
                              io:format("Broadcasting failed for path ~p to ~p ~n Reason: ~p~n",
                                        [Path, Topic, Reason]),
                              ok
                          end
                      end
                   end),
      ok
  end.

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
  logger:warning("Registering [procs, on_new_event, ~s] FILTER: ~p~n", [Topic, Filter]),
  khepri:register_trigger(Store, Id, Filter, [procs, on_new_event, Topic], PropOpts).

-spec setup_new_event_trigger(Store :: khepri:store(),
                              Id :: string(),
                              Filter :: khepri:filter()) ->
                               ok | {error, term()}.
setup_new_event_trigger(Store, Id, Filter) ->
  ok = put_on_new_event(Store, Id),
  ok = register_on_new_event(Store, Id, Filter),
  ok.

-spec setup_emitters(Store :: khepri:store(),
                     Id :: string(),
                     Filter :: khepri:filter(),
                     PoolSize :: integer()) ->
                      list().
setup_emitters(Store, Id, Filter, PoolSize) ->
  ok = setup_new_event_trigger(Store, Id, Filter),
  Emitters = emitter_group:persist_emitters(Store, Id, PoolSize),
  Emitters.

-spec get_event(Store :: khepri:store(), Path :: khepri_path:path()) ->
                 {ok, khepri:props()} | {error, term()}.
get_event(Store, Path) ->
  khepri:get(Store, Path).
