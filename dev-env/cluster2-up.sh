#! /bin/bash

## CLEAR ALL DATA
# echo "Clearing all data"
# sudo rm -rf /volume
docker-compose \
  -f ex-esdb-volumes2.yaml \
  -f ex-esdb-cluster2.yaml \
  --profile cluster \
  -p cluster2 \
  down

## CACHES
echo "Removing caches folder"
sudo rm -rf /volume/caches
echo "Creating caches folder"
sudo mkdir -p /volume/caches
# ExESDB Extended cluster data folders (nodes 10-11)
echo "removing ExESDB extended cluster data folders"
sudo rm -rf /volume/ex-esdb/data1[0-1]
echo "creating ExESDB extended cluster data folders"
sudo mkdir -p \
  /volume/ex-esdb/data10 \
  /volume/ex-esdb/data11

sudo chown "$USER" -R /volume/

docker-compose \
  -f ex-esdb-volumes2.yaml \
  -f ex-esdb-cluster2.yaml \
  --profile cluster \
  -p cluster2 \
  up \
  --remove-orphans \
  --build \
  -d
