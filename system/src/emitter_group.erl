-module(emitter_group).

-export([join/3, members/2]).

-spec join(atom(), string() | atom(), pid() | [pid()]) -> ok | {error, term()}.
join(Scope, Group, PidOrPids) ->
  pg:join(Scope, Group, PidOrPids).

-spec members(atom(), string() | atom()) -> [pid()].
members(Scope, Group) ->
  pg:get_members(Scope, Group).
