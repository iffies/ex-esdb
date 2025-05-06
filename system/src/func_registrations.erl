-module(func_registrations).

-export([register_emitter/2, build_filter/1, put_on_create_func/2, broadcast/3, group/1,
         pg_members/1, get_term_key/1, forward_to_local_msg/2, broadcast_msg/2]).

-include_lib("../deps/khepri/include/khepri.hrl").

-spec get_event(Store :: khepri:store(), Path :: khepri_path:path()) ->
                 {ok, khepri:props()} | {error, term()}.
get_event(Store, Path) ->
  khepri:get(Store, Path).

-spec put_on_create_func(Store :: khepri:store(), Stream :: string() | all) ->
                          ok | {error, term()}.
put_on_create_func(Store, Stream) when is_atom(Store) ->
  case khepri:put(Store,
                  [procs, on_new_event, Stream],
                  fun(Props) ->
                     case maps:get(path, Props, undefined) of
                       undefined -> ok;
                       Path ->
                         case get_event(Store, Path) of
                           {ok, undefined} -> ok;
                           {ok, Event} ->
                             broadcast(Store, Stream, Event),
                             ok;
                           {error, Reason} ->
                             io:format("Broadcasting failed for path ~p to Store:Stream ~p:~p ~n Reason: "
                                       "~p~n",
                                       [Path, Store, Stream, Reason]),
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

-spec build_filter(Stream :: string() | all) -> khepri:eventFilter() | {error, term()}.
build_filter(all) ->
  khepri_evf:tree([streams, #if_path_matches{regex = any}], #{on_actions => [create]});
build_filter(Stream) ->
  List = binary_to_list(Stream),
  case string:chr(List, $$) of
    0 ->
      {error, invalid_stream};
    DollarPos ->
      StreamUuid = string:substr(List, DollarPos + 1),
      khepri_evf:tree([streams, list_to_binary(StreamUuid)], #{on_actions => [create]})
  end.

-spec register_on_new_event(Store :: khepri:store(), Stream :: string() | all) -> ok.
register_on_new_event(Store, Stream) ->
  PropOpts =
    #{expect_specific_node => false,
      props_to_return =>
        [payload, payload_version, child_list_version, child_list_length, child_names],
      include_root_props => true},
  Filter = build_filter(Stream),
  khepri:register_trigger(Store, Stream, Filter, [procs, on_new_event], PropOpts).

-spec register_emitter(Store :: khepri:store(), Stream :: string() | all) ->
                        ok | {error, term()}.
register_emitter(Store, Stream) ->
  case put_on_create_func(Store, Stream) of
    ok ->
      register_on_new_event(Store, Stream);
    {error, Reason} ->
      {error, Reason}
  end.

broadcast(Store, Stream, Event) ->
  %% TODO: group(Topic)
  %% We must refine this messaging mechanism for instance by refining the topic
  %% to a format Store:all and Store:StreamUuid
  %% THIS IS A TEMPORARY SOLUTION
  Topic = list_to_binary(io_lib:format("~s:~s", [Store, Stream])),
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

get_term_key(Topic) when is_atom(Topic) ->
  iolist_to_binary(io_lib:format("~s_ex_esdb_emitters", [atom_to_list(Topic)]));
get_term_key(Topic) when is_binary(Topic) ->
  iolist_to_binary(io_lib:format("~s_ex_esdb_emitters", [binary_to_list(Topic)])).

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
