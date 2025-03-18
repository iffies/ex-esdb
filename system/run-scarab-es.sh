#! /bin/bash

echo "PATH=${PATH}"
echo "SCARAB_SEEDS=${SCARAB_SEEDS}"

sleep 10

which epmd

/system/bin/scarab_es start

# until epmd -names | grep ${SCARAB_SEEDS%% *}; do
#   sleep 1
# done
