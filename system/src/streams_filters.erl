-module(streams_filters).

-export([by_stream/1, by_event_type/1, by_event_pattern/1, by_event_payload/1]).

-include_lib("../deps/khepri/include/khepri.hrl").

-spec by_stream(Stream :: string()) -> khepri:filter() | {error, term()}.
by_stream(<<"$all">>) ->
  khepri_evf:tree([streams,
                   #if_path_matches{regex = any},
                   #if_all{conditions =
                             [#if_path_matches{regex = any}, #if_has_data{has_data = true}]}],
                  #{on_actions => [create]});
by_stream(Stream) ->
  List = binary_to_list(Stream),
  case string:chr(List, $$) of
    0 ->
      {error, invalid_stream};
    DollarPos ->
      StreamUuid = string:substr(List, DollarPos + 1),
      khepri_evf:tree([streams,
                       list_to_binary(StreamUuid),
                       #if_all{conditions =
                                 [#if_path_matches{regex = any}, #if_has_data{has_data = true}]}],
                      #{on_actions => [create]})
  end.

-spec by_event_type(EventType :: string()) -> khepri:filter().
by_event_type(EventType) ->
  by_event_pattern(#{event_type => EventType}).

-spec by_event_pattern(EventPattern :: map()) -> khepri:filter().
by_event_pattern(EventPattern) ->
  khepri_evf:tree([streams,
                   #if_path_matches{regex = any},
                   #if_all{conditions =
                             [#if_path_matches{regex = any},
                              #if_has_data{has_data = true},
                              #if_data_matches{pattern = EventPattern}]}],
                  #{on_actions => [create]}).

-spec by_event_payload(PayloadPattern :: map()) -> khepri:filter().
by_event_payload(PayloadPattern) ->
  khepri_evf:tree([streams,
                   #if_path_matches{regex = any},
                   #if_all{conditions =
                             [#if_path_matches{regex = any},
                              #if_has_data{has_data = true},
                              #if_data_matches{pattern = #{data => PayloadPattern}}]}],
                  #{on_actions => [create]}).
