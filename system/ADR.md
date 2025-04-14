# Architecture Decision Records

## 2025.04.13

### Each Store will contain separate branches for Streams, Subscriptions, and Projections

- `:streams` will be used to store the events that are being read from and written to the store.
- `:subscriptions` will be used to store the subscription information of so-called `Persistent Subscriptions`.
- `:projections` can best be thought of as stored procedures that are used to transform the events in the `Streams` into a different format or to enrich the streams with secondary or derived events, to name a few possible use cases.

Thus:

```mono
:khepri
  |
  +-:manage_orders
      |
      +-:streams
      |
      +-:subscriptions
      |
      +-:projections
```
