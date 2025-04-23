-module(func_registrations).

-export([register_emitter/3, build_filter/1, put_on_create_func/2]).

-include_lib("../deps/khepri/include/khepri.hrl").

-spec get_event(Store :: khepri:store(), Path :: khepri_path:path()) ->
                 {ok, khepri:props()} | {error, term()}.
get_event(Store, Path) ->
  khepri:get(Store, Path).

-spec put_on_create_func(PubSub :: atom(), Store :: khepri:store()) -> ok.
put_on_create_func(PubSub, Store) when is_atom(PubSub), is_atom(Store) ->
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
                             broadcast_pg(PubSub, Topic, Event),
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

-spec broadcast_pg(PubSub :: atom(), Topic :: binary(), Event :: khepri:payload()) -> ok.
broadcast_pg(PubSub, Topic, Event) ->
  case pg:get_members(PubSub, Topic) of
    [] ->
      io:format("!!!!! No members for topic ~p !!!!~n", [Topic]),
      ok;
    Members ->
      io:format("!!!!! Broadcasting event ~p to topic ~p to ~p members !!!!~n",
                [Event, Topic, length(Members)]),
      lists:foreach(fun(Member) -> Member ! {event_emitted, Event} end, Members),
      ok
  end.
