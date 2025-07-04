#! /bin/bash

docker-compose \
  -f ex-esdb-volumes2.yaml \
  -f ex-esdb-cluster2.yaml \
  --profile cluster \
  -p cluster2 \
  down
