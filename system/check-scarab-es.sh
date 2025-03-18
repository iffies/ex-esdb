#!/bin/bash
# check-scarab-es.sh

epmd -names | grep -q scarab_es &&
  erl -noshell \
    -name "health@$(hostname)" \
    -setcookie "$SCARAB_COOKIE" \
    -eval 'case khepri:is_connected() of true -> halt(0); _ -> halt(1) end.'
