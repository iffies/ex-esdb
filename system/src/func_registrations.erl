-module(func_registrations).

-export([register_emitter/2]).

-spec reg_on_create(PubSub :: atom(), Store :: khepri:store()) -> ok | {error, term()}.
reg_on_create(PubSub, Store) ->
  khepri:put(Store,
             [procs, on_new_event],
             fun(Props) -> broadcast(PubSub, Store, Props) end).

-spec register_emitter(PubSub :: atom(), Store :: khepri:store()) -> ok | {error, term()}.
register_emitter(PubSub, Store) ->
  case reg_on_create(PubSub, Store) of
    ok ->
      NewEventFilter = khepri_evf:tree([Store, streams], #{on_actions => [create]}),
      khepri:register_trigger(Store, on_new_event, NewEventFilter, [procs, on_new_event]);
    {error, Reason} ->
      io:format("Registration failed: ~p~n", [Reason]),
      {error, Reason}
  end.

-spec broadcast(PubSub :: atom(), Store :: khepri:store(), Props :: khepri:props()) -> ok.
broadcast(PubSubName, Topic, Message) ->
  Members = pg:get_members(PubSubName, Topic),
  lists:foreach(fun(Member) -> Member ! {message, Topic, Message} end, Members),
  ok.
