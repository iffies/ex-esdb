#! /bin/bash

docker-compose -f ex-esdb-cluster.yaml \
  --profile cluster \
  -p cluster \
  down
