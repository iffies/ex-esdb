#! /bin/bash

## CLEAR ALL DATA
# echo "Clearing all data"
# sudo rm -rf /volume
docker-compose -f ex-esdb-cluster.yaml \
  down
