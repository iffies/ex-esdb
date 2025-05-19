-module(func_registrations).

-export([register_emitter/3, put_emitters/4, put_on_new_event/2, get_on_new_event/2,
         broadcast/2, emitter/2, forward_to_local_msg/2, broadcast_msg/2, get_topic_emitters_key/2,
         register_on_new_event/3, topic/2]).

-spec get_on_new_event(Store :: khepri:store(), Id :: string()) -> ok | {error, term()}.
get_on_new_event(Store, Id) when is_atom(Store) ->
  Topic = topic(Store, Id),
  khepri:get(Store, [procs, on_new_event, Topic]).

-spec put_on_new_event(Store :: khepri:store(), Id :: string()) -> ok | {error, term()}.
put_on_new_event(Store, Id) when is_atom(Store) ->
  Topic = topic(Store, Id),
  case khepri:exists(Store, [procs, on_new_event, Topic]) of
    true ->
      ok;
    false ->
      khepri:put(Store,
                 [procs, on_new_event, Topic],
                 fun(Props) ->
                    logger:warning("[procs, on_new_event, ~s] was called with Props: ~p~n",
                                   [Topic, Props]),
                    case maps:get(path, Props, undefined) of
                      undefined -> ok;
                      Path ->
                        case get_event(Store, Path) of
                          {ok, undefined} -> ok;
                          {ok, Event} ->
                            broadcast(Topic, Event),
                            ok;
                          {error, Reason} ->
                            io:format("Broadcasting failed for path ~p to ~p ~n Reason: ~p~n",
                                      [Path, Topic, Reason]),
                            ok
                        end
                    end
                 end)
  end.

-spec register_on_new_event(Store :: khepri:store(),
                            Id :: string(),
                            Filter :: khepri:filter()) ->
                             ok | {error, term()}.
register_on_new_event(Store, Id, Filter) ->
  Topic = topic(Store, Id),
  PropOpts =
    #{expect_specific_node => false,
      props_to_return =>
        [payload, payload_version, child_list_version, child_list_length, child_names],
      include_root_props => true},
  logger:warning("Registering [procs, on_new_event] for ~p:~s~n FILTER: ~p~n",
                 [Store, Id, Filter]),
  khepri:register_trigger(Store, Id, Filter, [procs, on_new_event, Topic], PropOpts).

-spec put_emitters(Store :: khepri:store(),
                   Id :: string(),
                   Filter :: khepri:filter(),
                   PoolSize :: integer()) ->
                    list().
put_emitters(Store, Id, Filter, PoolSize) ->
  ok = register_emitter(Store, Id, Filter),
  % Generate a list of emitter names
  EmitterList =
    [list_to_atom(lists:flatten(
                    io_lib:format("~s:~s_emitter_~p", [Store, Id, Number])))
     || Number <- lists:seq(1, PoolSize)],
  % Extract all but the first element (discarding the head)
  % and prepend the main emitter name
  Emitters =
    [list_to_atom(lists:flatten(
                    io_lib:format("~s:~s_emitter", [Store, Id])))
     | EmitterList],
  Key = get_topic_emitters_key(Store, Id),
  persistent_term:put(Key, list_to_tuple(Emitters)),
  Emitters.

-spec register_emitter(Store :: khepri:store(),
                       Id :: string(),
                       Filter :: khepri:filter()) ->
                        ok | {error, term()}.
register_emitter(Store, Id, Filter) ->
  case put_on_new_event(Store, Id) of
    ok ->
      register_on_new_event(Store, Id, Filter),
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

broadcast(Topic, Event) ->
  logger:warning("Broadcasting ~p to ~p~n", [Event, Topic]),
  case emitter_group:members('Elixir.Phoenix.PubSub', Topic) of
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

% get_term_key(Topic) when is_atom(Topic) ->
%   iolist_to_binary(io_lib:format("~s_emitters", [atom_to_list(Topic)]));
% get_term_key(Topic) when is_binary(Topic) ->
%   iolist_to_binary(io_lib:format("~s_emitters", [binary_to_list(Topic)])).
-spec topic(Store :: atom(), Id :: string()) -> binary().
topic(Store, <<"$all">>) ->
  iolist_to_binary(io_lib:format("~s:$all", [Store]));
topic(Store, Id) ->
  iolist_to_binary(io_lib:format("~s:~s", [Store, Id])).

get_topic_emitters_key(Store, Id) ->
  {Store, Id, emitters}.

emitter(Store, Id) ->
  Key = get_topic_emitters_key(Store, Id),
  Emitters = persistent_term:get(Key),
  Size = tuple_size(Emitters),
  Random = rand:uniform(Size),
  erlang:element(Random, Emitters).

-spec forward_to_local_msg(Topic :: binary(), Event :: map()) -> tuple().
forward_to_local_msg(Topic, Event) ->
  {forward_to_local, Topic, Event}.

-spec broadcast_msg(Topic :: binary(), Event :: map()) -> tuple().
broadcast_msg(Topic, Event) ->
  {broadcast, Topic, Event}.

-spec get_event(Store :: khepri:store(), Path :: khepri_path:path()) ->
                 {ok, khepri:props()} | {error, term()}.
get_event(Store, Path) ->
  khepri:get(Store, Path).
