-module(func_registrations).

-export([register_emitter/3]).

-spec reg_on_create(PubSub :: atom(), Store :: khepri:store()) -> ok | {error, term()}.
reg_on_create(PubSub, Store) when is_atom(PubSub), is_atom(Store) ->
  Topic = atom_to_binary(Store, utf8),
  khepri:put(Store,
             [procs, on_new_event],
             fun(Props) -> broadcast_pg(PubSub, Topic, Props) end).

-spec register_emitter(Store :: khepri:store(),
                       PubSub :: atom(),
                       StreamUuid :: khepri:tree()) ->
                        ok | {error, term()}.
register_emitter(Store, PubSub, StreamUuid) ->
  case reg_on_create(PubSub, Store) of
    ok ->
      logger:warning("Registered on_new_event TRIGGER for store ~p~n", [Store]),
      NewEventFilter = khepri_evf:tree([Store, streams, StreamUuid], #{on_actions => [create]}),
      khepri:register_trigger(Store, on_new_event, NewEventFilter, [procs, on_new_event]);
    {error, Reason} ->
      logger:error("Khepri TRIGGER registration failed: ~p~n", [Reason]),
      {error, Reason}
  end.

-spec broadcast_pg(PubSub :: atom(), Topic :: binary(), Props :: khepri:props()) -> ok.
broadcast_pg(PubSub, Topic, Props) ->
  Members = pg:get_members(PubSub, Topic),
  lists:foreach(fun(Member) -> Member ! {event_emitted, Props} end, Members),
  ok.
