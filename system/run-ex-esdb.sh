#! /bin/bash

echo "PATH=${PATH}"
echo "EX_ESDB_SEED_NODES=${EX_ESDB_SEED_NODES}"

sleep 10

which epmd

/system/bin/ex_esdb start

# until epmd -names | grep ${SCARAB_SEEDS%% *}; do
#   sleep 1
# done
