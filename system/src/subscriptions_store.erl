-module(subscriptions_store).

-export([get_subscription/2, put_subscription/2, delete_subscription/2, key/1]).

get_subscription(Store, Path) ->
  case khepri:get(Store, [subscriptions, Path]) of
    {ok, Subscription} ->
      Subscription;
    {error, _Reason} ->
      nil
  end.

key(Subscription) ->
  #{type := Type,
    selector := Selector,
    subscription_name := SubscriptionName} =
    Subscription,
  erlang:phash2({Type, Selector, SubscriptionName}).

put_subscription(Store, Subscription) ->
  Key = key(Subscription),
  case khepri:exists(Store, [subscriptions, Key]) of
    true ->
      ok;
    false ->
      ok = khepri:put(Store, [subscriptions, Key], Subscription),
      ok
  end.

delete_subscription(Store, Subscription) ->
  Key = key(Subscription),
  case khepri:exists(Store, [subscriptions, Key]) of
    true ->
      ok = khepri:delete(Store, [subscriptions, Key]),
      ok;
    false ->
      ok
  end.
