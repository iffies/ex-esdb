-module(func_registrations).

-export([register_emitter/2, build_filter/1, put_on_create_func/1, broadcast/2, group/1,
         pg_members/1]).

-include_lib("../deps/khepri/include/khepri.hrl").

-spec get_event(Store :: khepri:store(), Path :: khepri_path:path()) ->
                 {ok, khepri:props()} | {error, term()}.
get_event(Store, Path) ->
  khepri:get(Store, Path).

-spec put_on_create_func(Store :: khepri:store()) -> ok.
put_on_create_func(Store) when is_atom(Store) ->
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
                             broadcast(Topic, Event),
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

-spec register_emitter(Store :: khepri:store(), StreamUuid :: khepri:tree()) ->
                        ok | {error, term()}.
register_emitter(Store, StreamUuid) ->
  case put_on_create_func(Store) of
    ok ->
      NewEventFilter = build_filter(StreamUuid),
      register_on_new_event(Store, NewEventFilter);
    {error, Reason} ->
      {error, Reason}
  end.

broadcast(Topic, Event) ->
  case pg_members(group(Topic)) of
    {error, {no_such_group, _}} ->
      {error, no_such_group};
    Members ->
      Message = forward_to_local(Topic, Event),
      lists:foreach(fun(Pid) ->
                       if node(Pid) =/= node() -> Pid ! Message;
                          true -> ok
                       end
                    end,
                    Members)
  end.

group(Store) ->
  Key =
    lists:flatten(
      io_lib:format("~p_ex_esdb_emitters", [Store])),
  Groups = persistent_term:get(Key, groups),
  erlang:element(Groups, erlang:phash2(self(), tuple_size(Groups))).

pg_members(Group) ->
  pg:get_members('Phoenix.PubSub', Group).

forward_to_local(Topic, Event) ->
  {forward_to_local, Topic, Event}.
