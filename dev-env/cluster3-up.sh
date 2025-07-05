#! /bin/bash

## CLEAR ALL DATA
# echo "Clearing all data"
# sudo rm -rf /volume
docker-compose \
  -f ex-esdb-volumes3.yaml \
  -f ex-esdb-cluster3.yaml \
  --profile cluster \
  -p cluster3 \
  down

## CACHES
echo "Removing caches folder"
sudo rm -rf /volume/caches
echo "Creating caches folder"
sudo mkdir -p /volume/caches
# ExESDB Massive cluster data folders (nodes 20-27)
echo "removing ExESDB massive cluster data folders"
sudo rm -rf /volume/ex-esdb/data2[0-7]
echo "creating ExESDB massive cluster data folders"
sudo mkdir -p \
  /volume/ex-esdb/data20 \
  /volume/ex-esdb/data21 \
  /volume/ex-esdb/data22 \
  /volume/ex-esdb/data23 \
  /volume/ex-esdb/data24 \
  /volume/ex-esdb/data25 \
  /volume/ex-esdb/data26 \
  /volume/ex-esdb/data27

sudo chown "$USER" -R /volume/

docker-compose \
  -f ex-esdb-volumes3.yaml \
  -f ex-esdb-cluster3.yaml \
  --profile cluster \
  -p cluster3 \
  up \
  --remove-orphans \
  --build \
  -d
