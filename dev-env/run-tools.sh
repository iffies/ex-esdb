#! /bin/bash

## CLEAR ALL DATA
# echo "Clearing all data"
# sudo rm -rf /volume

## CACHES
echo "Creating caches folder"
sudo mkdir -p /volume/caches
# SCARAB
echo "creating scarab data folders"
sudo mkdir -p /volume/scarab/data0 /volume/scarab/data1 /volume/scarab/data2
## EXCALIDRAW
echo "Creating excalidraw folder"
sudo mkdir -p /volume/excalidraw/data

sudo chown "$USER" -R /volume/

docker-compose -f livebook.yml \
  -f xoom-designer.yml \
  -f excalidraw.yml \
  -f networks.yml \
  down

docker-compose -f livebook.yml \
  -f xoom-designer.yml \
  -f excalidraw.yml \
  -f networks.yml \
  up --remove-orphans --build $1
