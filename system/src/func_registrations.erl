-module(func_registrations).

-export([register_emitter/3, build_filter/1, put_on_create_func/2]).

-spec put_on_create_func(PubSub :: atom(), Store :: khepri:store()) -> ok.
put_on_create_func(PubSub, Store) when is_atom(PubSub), is_atom(Store) ->
  Topic = atom_to_binary(Store, utf8),
  case khepri:put(Store,
                  [procs, on_new_event],
                  fun(Props) ->
                     io:format("Broadcasting ~n Event: ~p~n to Topic: ~p~n on Bus: ~p~n",
                               [Props, Topic, PubSub])
                  end)
  of
    %                     ,                     broadcast_pg(PubSub, Topic, Props)
    ok ->
      logger:warning("PUT broadcast_pg function in store ~p~n", [Store]),
      ok;
    {error, Reason} ->
      logger:error("PUT broadcast_pg function in store ~p failed: ~p~n", [Store, Reason]),
      {error, Reason}
  end.

-spec build_filter(StreamUuid :: khepri:tree()) -> eventFilter.
build_filter(StreamUuid) ->
  case StreamUuid of
    all ->
      khepri_evf:tree([streams], #{on_actions => [create]});
    _ ->
      khepri_evf:tree([streams, StreamUuid], #{on_actions => [create]})
  end.

-spec register_on_new_event(Store :: khepri:store(), Filter :: eventFilter) -> ok.
register_on_new_event(Store, Filter) ->
  PropOpts =
    #{expect_specific_node => false,
      props_to_return => [payload, payload_version],
      include_root_props => true},
  case khepri:register_trigger(Store, on_new_event, Filter, [procs, on_new_event], PropOpts)
  of
    ok ->
      logger:warning("Registered on_new_event trigger in store ~p~n", [Store]),
      ok;
    {error, Reason} ->
      logger:error("Registered on_new_event trigger in store ~p failed: ~p~n", [Store, Reason]),
      {error, Reason}
  end.

-spec register_emitter(Store :: khepri:store(),
                       PubSub :: atom(),
                       StreamUuid :: khepri:tree()) ->
                        ok | {error, term()}.
register_emitter(Store, PubSub, StreamUuid) ->
  case put_on_create_func(PubSub, Store) of
    ok ->
      NewEventFilter = build_filter(StreamUuid),
      register_on_new_event(Store, NewEventFilter);
    {error, Reason} ->
      {error, Reason}
  end.

-spec broadcast_pg(PubSub :: atom(), Topic :: binary(), Props :: khepri:props()) -> ok.
broadcast_pg(PubSub, Topic, Props) ->
  logger:debug("Broadcasting event ~p to topic ~p~n", [Props, Topic]),
  Members = pg:get_members(PubSub, Topic),
  lists:foreach(fun(Member) -> Member ! {event_emitted, Props} end, Members),
  ok.

