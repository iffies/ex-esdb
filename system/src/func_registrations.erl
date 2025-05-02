-module(func_registrations).

-export([register_emitter/2, build_filter/1, put_on_create_func/1, broadcast/2, group/1,
         pg_members/1, get_term_key/1, forward_to_local_msg/2, broadcast_msg/2]).

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
  %% TODO: group(Topic)
  %% We must refine this messaging mechanism for instance by refining the topic
  %% to a format Store:all and Store:StreamUuid
  %% THIS IS A TEMPORARY SOLUTION
  case pg_members(get_term_key(Topic)) of
    {error, {no_such_group, _}} ->
      io:format("NO_GROUP ~p~n", [Topic]),
      {error, no_such_group};
    Members ->
      Message = broadcast_msg(Topic, Event),
      lists:foreach(fun(Pid) ->
                       io:format("SENDING Message ~p to ~p~n", [Message, Pid]),
                       Pid ! Message
                    end,
                    Members)
  end.

get_term_key(Store) when is_atom(Store) ->
  iolist_to_binary(io_lib:format("~s_ex_esdb_emitters", [atom_to_list(Store)]));
get_term_key(Store) when is_binary(Store) ->
  iolist_to_binary(io_lib:format("~s_ex_esdb_emitters", [binary_to_list(Store)])).

group(Store) ->
  Key = get_term_key(Store),
  Groups = persistent_term:get(Key, groups),
  Size = tuple_size(Groups),
  Random = rand:uniform(Size),
  erlang:element(Random, Groups).

pg_members(Group) ->
  pg:get_members('Elixir.Phoenix.PubSub', Group).

forward_to_local_msg(Topic, Event) ->
  {forward_to_local, Topic, Event}.

broadcast_msg(Topic, Event) ->
  {broadcast, Topic, Event}.
