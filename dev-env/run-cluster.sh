#! /bin/bash

## CLEAR ALL DATA
# echo "Clearing all data"
# sudo rm -rf /volume

## CACHES
echo "Creating caches folder"
sudo mkdir -p /volume/caches
# SCARAB
echo "creating scarab data folders"
sudo mkdir -p \
  /volume/scarab/data0 \
  /volume/scarab/data1 \
  /volume/scarab/data2 \
  /volume/scarab/data3 \
  /volume/scarab/data4

sudo chown "$USER" -R /volume/

docker-compose -f scarab-custer-1.yaml \
  down

docker-compose -f scarab-custer-1.yaml \
  up --remove-orphans --build $1
