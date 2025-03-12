-module(func_registrations).

-export([register_emitter/2]).

%% Use MFA pattern instead of anonymous functions for serialization
-spec register_oncreate_fun(PubSub :: atom(), Store :: khepri:store()) ->
                             ok | {error, term()}.
register_oncreate_fun(PubSub, Store) ->
  %% Store MFA tuple instead of anonymous function
  khepri:put(Store,
             [procs, on_new_event],
             fun(Props) -> broadcast(PubSub, Store, Props) end).  % Use pattern-matched props

-spec register_emitter(PubSub :: atom(), Store :: khepri:store()) -> ok | {error, term()}.
register_emitter(PubSub, Store) ->
  case register_oncreate_fun(PubSub, Store) of
    ok ->
      %% Proper trigger registration with path pattern
      NewEventFilter = khepri_evf:tree([Store, streams], #{on_actions => [create]}),
      khepri:register_trigger(Store, on_new_event, NewEventFilter, [procs, on_new_event]);
    {error, Reason} ->  %% Match error tuple explicitly
      io:format("Registration failed: ~p~n", [Reason]),
      {error, Reason}
  end.

broadcast(PubSubName, Topic, Message) ->
  % Convert Erlang terms to Elixir terms
  %    PubSubNameElixir = erlang:binary_to_term(term_to_binary(PubSubName)),
  %  TopicElixir = erlang:binary_to_term(term_to_binary(Topic)),
  %  MessageElixir = erlang:binary_to_term(term_to_binary(Message)),
  % Call the Phoenix.PubSub.broadcast/3 function
  Members = pg:get_members(PubSubName, Topic),
  lists:foreach(fun(Member) -> Member ! {message, Topic, Message} end, Members),
  ok.
