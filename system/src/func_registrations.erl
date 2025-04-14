-module(func_registrations).

-export([register_pg_emitter/2]).

-spec reg_on_create(PubSub :: atom(), Store :: khepri:store()) -> ok | {error, term()}.
reg_on_create(PubSub, Store) ->
  khepri:put(Store,
             [procs, on_new_event],
             fun(Props) -> broadcast(PubSub, Store, Props) end).

-spec register_pg_emitter(Store :: khepri:store(), PubSub :: atom()) ->
                           ok | {error, term()}.
register_pg_emitter(Store, PubSub) ->
  case reg_on_create(PubSub, Store) of
    ok ->
      logger:warning("Registered on_new_event TRIGGER for store ~p~n", [Store]),
      NewEventFilter = khepri_evf:tree([Store, streams], #{on_actions => [create]}),
      khepri:register_trigger(Store, on_new_event, NewEventFilter, [procs, on_new_event]);
    {error, Reason} ->
      logger:error("Khepri TRIGGER registration failed: ~p~n", [Reason]),
      {error, Reason}
  end.

-spec broadcast(PubSub :: atom(), Store :: khepri:store(), Props :: khepri:props()) -> ok.
broadcast(PubSub, Store, Message) ->
  %  Topic = atom_to_binary(Store, utf8),
  %  Members = pg:get_members(PubSub, Topic),
  Members = pg:get_members(PubSub, Store),
  lists:foreach(fun(Member) -> Member ! {event_emitted, Store, Message} end, Members),
  ok.
