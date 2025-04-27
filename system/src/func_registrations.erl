-module(func_registrations).

-export([register_emitter/4, build_filter/1, put_on_create_func/3, broadcast/3]).

-include_lib("../deps/khepri/include/khepri.hrl").

-spec get_event(Store :: khepri:store(), Path :: khepri_path:path()) ->
                 {ok, khepri:props()} | {error, term()}.
get_event(Store, Path) ->
  khepri:get(Store, Path).

-spec put_on_create_func(PubSub :: atom(),
                         Store :: khepri:store(),
                         Dispatcher :: atom()) ->
                          ok.
put_on_create_func(PubSub, Store, Dispatcher)
  when is_atom(PubSub), is_atom(Store), is_atom(Dispatcher) ->
  case khepri:put(Store,
                  [procs, on_new_event],
                  fun(Props) ->
                     Topic = atom_to_binary(Store, utf8),
                     case maps:get(path, Props, undefined) of
                       undefined -> ok;
                       Path ->
                         case get_event(Store, Path) of
                           {ok, undefined} -> ok;
                           {ok, Event} ->
                             io:format("Broadcasting event ~p~n~n to topic ~p~n", [Event, Topic]),
                             'Elixir.Phoenix.PubSub':broadcast(PubSub, Topic, Event),
                             % broadcast(PubSub, Topic, Event, Dispatcher),
                             ok;
                           {error, Reason} ->
                             io:format("Broadcasting failed for path ~p to topic ~p~n Reason: ~p~n",
                                       [Path, Topic, Reason]),
                             ok
                         end
                     end
                  end)
  of
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
      khepri_evf:tree([streams, #if_path_matches{regex = any}], #{on_actions => [create]});
    _ ->
      khepri_evf:tree([streams, StreamUuid], #{on_actions => [create]})
  end.

-spec register_on_new_event(Store :: khepri:store(), Filter :: eventFilter) -> ok.
register_on_new_event(Store, Filter) ->
  PropOpts =
    #{expect_specific_node => false,
      props_to_return =>
        [payload, payload_version, child_list_version, child_list_length, child_names],
      include_root_props => true},
  khepri:register_trigger(Store, on_new_event, Filter, [procs, on_new_event], PropOpts).

-spec register_emitter(Store :: khepri:store(),
                       PubSub :: atom(),
                       StreamUuid :: khepri:tree(),
                       Dispatcher :: atom()) ->
                        ok | {error, term()}.
register_emitter(Store, PubSub, StreamUuid, Dispatcher) ->
  case put_on_create_func(PubSub, Store, Dispatcher) of
    ok ->
      NewEventFilter = build_filter(StreamUuid),
      register_on_new_event(Store, NewEventFilter);
    {error, Reason} ->
      {error, Reason}
  end.

% broadcast(PubSub, Topic, Event, Dispatcher) ->
%   {ok, {Adapter, Name}} = 'Elixir.Registry':meta(PubSub, pubsub),
%
%   case Adapter:broadcast(Name, Topic, Event, Dispatcher) of
%     ok ->
%       dispatch(PubSub, none, Topic, Event, Dispatcher),
%       ok;
%     _ ->
%       ok
%   end.

broadcast(PubSub, Topic, Event) when is_atom(PubSub), is_binary(Topic), is_map(Event) ->
  'Elixir.Phoenix.PubSub':broadcast(PubSub, Topic, Event).
