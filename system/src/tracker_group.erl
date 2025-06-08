-module(tracker_group).

-export([join/3, members/2, group_key/2, leave/3, notify_created/3, notify_deleted/3]).

-spec join(Store :: atom(), Selector :: atom(), PidOrPids :: pid() | [pid()]) -> ok.
join(Store, Selector, PidOrPids) ->
  Group = group_key(Store, Selector),
  ok = pg:join('Elixir.Phoenix.PubSub', Group, PidOrPids),
  ok.

-spec members(Store :: atom(), Selector :: atom()) -> [pid()].
members(Store, Selector) ->
  Group = group_key(Store, Selector),
  pg:get_members('Elixir.Phoenix.PubSub', Group).

-spec group_key(Store :: atom(), Selector :: atom()) -> integer().
group_key(Store, Selector) ->
  erlang:phash2({Store, Selector, trackers}).

-spec leave(Store :: atom(), Selector :: atom(), PidOrPids :: pid() | [pid()]) -> ok.
leave(Store, Selector, PidOrPids) ->
  Group = group_key(Store, Selector),
  ok = pg:leave('Elixir.Phoenix.PubSub', Group, PidOrPids),
  ok.

created_msg(Store, Selector, Data) ->
  {feature_created, Store, Selector, Data}.

deleted_msg(Store, Selector, Data) ->
  {feature_deleted, Store, Selector, Data}.

-spec notify_created(Store :: atom(), Selector :: atom(), Data :: map()) -> ok.
notify_created(Store, Selector, Data) ->
  Msg = created_msg(Store, Selector, Data),
  Pids = members(Store, Selector),
  lists:foreach(fun(Pid) -> Pid ! Msg end, Pids).

-spec notify_deleted(Store :: atom(), Selector :: atom(), Data :: map()) -> ok.
notify_deleted(Store, Selector, Data) ->
  Msg = deleted_msg(Store, Selector, Data),
  Pids = members(Store, Selector),
  lists:foreach(fun(Pid) -> Pid ! Msg end, Pids).
