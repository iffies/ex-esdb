#! /bin/bash

docker-compose \
  -f ex-esdb-volumes3.yaml \
  -f ex-esdb-cluster3.yaml \
  --profile cluster \
  -p cluster3 \
  down
