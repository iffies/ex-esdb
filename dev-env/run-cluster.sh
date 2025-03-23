#! /bin/bash

## CLEAR ALL DATA
# echo "Clearing all data"
# sudo rm -rf /volume

## CACHES
echo "Creating caches folder"
sudo mkdir -p /volume/caches
# SCARAB
echo "creating Scarabes data folders"
sudo mkdir -p \
  /volume/scarabes/data0 \
  /volume/scarabes/data1 \
  /volume/scarabes/data2 \
  /volume/scarabes/data3 \
  /volume/scarabes/data4

sudo chown "$USER" -R /volume/

docker-compose -f scarabes-cluster.yaml \
  down

docker-compose -f scarabes-cluster.yaml \
  up --remove-orphans --build $1
