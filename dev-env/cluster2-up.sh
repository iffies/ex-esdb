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
# ExESDB cluster2 data folders
echo "removing ExESDB cluster2 data folders"
sudo rm -rf /volume/ex-esdb/data1[0-7]
echo "creating ExESDB cluster2 data folders"
sudo mkdir -p \
  /volume/ex-esdb/data10 \
  /volume/ex-esdb/data11 \
  /volume/ex-esdb/data12 \
  /volume/ex-esdb/data13 \
  /volume/ex-esdb/data14 \
  /volume/ex-esdb/data15 \
  /volume/ex-esdb/data16 \
  /volume/ex-esdb/data17

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
