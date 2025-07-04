#! /bin/bash

echo "PATH=${PATH}"
echo "EX_ESDB_COOKIE=${EX_ESDB_COOKIE}"

# # Set Erlang cookie from environment variable
# if [ -n "$EX_ESDB_COOKIE" ]; then
#   echo "Setting Erlang cookie from environment variable"
#   echo "$EX_ESDB_COOKIE" >/root/.erlang.cookie
#   chmod 400 /root/.erlang.cookie
# else
#   echo "Warning: EX_ESDB_COOKIE not set"
# fi
#
# echo "stored COOKIE:"
# cat /root/.erlang.cookie
#
sleep 10

exec /system/bin/ex_esdb start
